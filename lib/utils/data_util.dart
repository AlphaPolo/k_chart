import 'dart:math';

import '../entity/index.dart';

/// DMI / ADX / ADXR 計算（Wilder 平滑）
/// 輸入：open/high/low/close 同長度、按時間遞增（舊→新）
/// 輸出：與輸入等長的 List，前期不足 N 的位置為 null。
class DmiResult {
  final List<double?> plusDI; // +DI
  final List<double?> minusDI; // -DI
  final List<double?> adx;    // ADX
  final List<double?> adxr;   // ADXR（若 withAdxr=false，仍回傳等長但都為 null）
  const DmiResult(this.plusDI, this.minusDI, this.adx, this.adxr);
}

DmiResult _computeDMI({
  required List<double> high,
  required List<double> low,
  required List<double> close,
  int period = 14,
  bool withAdxr = true,
}) {
  assert(high.length == low.length && low.length == close.length && high.isNotEmpty);
  final n = high.length;
  final plusDM = List<double>.filled(n, 0);
  final minusDM = List<double>.filled(n, 0);
  final tr = List<double>.filled(n, 0);

  for (int i = 1; i < n; i++) {
    final up = high[i] - high[i - 1];
    final down = low[i - 1] - low[i];
    plusDM[i] = (up > 0 && up > down) ? up : 0.0;
    minusDM[i] = (down > 0 && down > up) ? down : 0.0;
    final tr1 = high[i] - low[i];
    final tr2 = (high[i] - close[i - 1]).abs();
    final tr3 = (low[i] - close[i - 1]).abs();
    tr[i] = [tr1, tr2, tr3].reduce((a, b) => a > b ? a : b);
  }

  double smTR = 0, smPDM = 0, smMDM = 0;
  for (int i = 1; i <= period && i < n; i++) {
    smTR += tr[i];
    smPDM += plusDM[i];
    smMDM += minusDM[i];
  }

  final pdi = List<double?>.filled(n, null);
  final mdi = List<double?>.filled(n, null);
  final dx = List<double?>.filled(n, null);
  final adx = List<double?>.filled(n, null);
  final adxr = List<double?>.filled(n, null);

  if (n <= period) {
    return DmiResult(pdi, mdi, adx, adxr);
  }

  // 第一個可用點（i=period）用簡單均值初始化
  pdi[period] = smTR == 0 ? 0 : 100.0 * (smPDM / smTR);
  mdi[period] = smTR == 0 ? 0 : 100.0 * (smMDM / smTR);
  dx[period] = ((pdi[period]! - mdi[period]!).abs()) /
      ((pdi[period]! + mdi[period]!).abs() == 0 ? 1e-12 : (pdi[period]! + mdi[period]!)) *
      100.0;

  // 之後用 Wilder 平滑
  for (int i = period + 1; i < n; i++) {
    smTR = smTR - (smTR / period) + tr[i];
    smPDM = smPDM - (smPDM / period) + plusDM[i];
    smMDM = smMDM - (smMDM / period) + minusDM[i];
    final double curP = smTR == 0 ? 0 : 100.0 * (smPDM / smTR);
    final double curM = smTR == 0 ? 0 : 100.0 * (smMDM / smTR);
    pdi[i] = curP;
    mdi[i] = curM;
    final denom = (curP + curM).abs();
    dx[i] = denom == 0 ? 0 : 100.0 * (curP - curM).abs() / denom;
  }

  // ADX: 對 DX 再做 Wilder 平滑
  double? adxSeed;
  double adxSm = 0;
  int cnt = 0;
  for (int i = 0; i < n; i++) {
    if (dx[i] == null) continue;
    if (adxSeed == null) {
      // 取接下來的 period 個 DX 的平均作為 seed
      final start = i;
      final end = (i + period).clamp(0, n);
      final slice = dx.sublist(start, end).whereType<double>();
      if (slice.length == period) {
        adxSeed = slice.reduce((a, b) => a + b) / period;
        adx[i + period - 1] = adxSeed; // 第一次可用點
        cnt = i + period - 1;
        adxSm = adxSeed;
      }
      continue;
    }
    if (i <= cnt) continue; // 尚未到下一點
    adxSm = adxSm - (adxSm / period) + (dx[i] ?? 0);
    adx[i] = adxSm;
  }

  if (withAdxr) {
    for (int i = 0; i < n; i++) {
      final j = i - period;
      if (j >= 0 && adx[i] != null && adx[j] != null) {
        adxr[i] = (adx[i]! + adx[j]!) / 2.0;
      }
    }
  }

  return DmiResult(pdi, mdi, adx, adxr);
}

class DataUtil {
  static calculate(List<KLineEntity> dataList,
      [List<int> maDayList = const [5, 10, 20], int n = 20, k = 2]) {
    calcMA(dataList, maDayList);
    calcBOLL(dataList, n, k);
    calcVolumeMA(dataList);
    calcKDJ(dataList);
    calcMACD(dataList);
    calcRSI(dataList);
    calcWR(dataList);
    calcCCI(dataList);
  }

  static calcMA(List<KLineEntity> dataList, List<int> maDayList) {
    List<double> ma = List<double>.filled(maDayList.length, 0);

    if (dataList.isNotEmpty) {
      for (int i = 0; i < dataList.length; i++) {
        KLineEntity entity = dataList[i];
        final closePrice = entity.close;
        entity.maValueList = List<double>.filled(maDayList.length, 0);

        for (int j = 0; j < maDayList.length; j++) {
          ma[j] += closePrice;
          if (i == maDayList[j] - 1) {
            entity.maValueList?[j] = ma[j] / maDayList[j];
          } else if (i >= maDayList[j]) {
            ma[j] -= dataList[i - maDayList[j]].close;
            entity.maValueList?[j] = ma[j] / maDayList[j];
          } else {
            entity.maValueList?[j] = 0;
          }
        }
      }
    }
  }

  static void calcBOLL(List<KLineEntity> dataList, int n, int k) {
    _calcBOLLMA(n, dataList);
    for (int i = 0; i < dataList.length; i++) {
      KLineEntity entity = dataList[i];
      if (i >= n) {
        double md = 0;
        for (int j = i - n + 1; j <= i; j++) {
          double c = dataList[j].close;
          double m = entity.BOLLMA!;
          double value = c - m;
          md += value * value;
        }
        md = md / (n - 1);
        md = sqrt(md);
        entity.mb = entity.BOLLMA!;
        entity.up = entity.mb! + k * md;
        entity.dn = entity.mb! - k * md;
      }
    }
  }

  static void _calcBOLLMA(int day, List<KLineEntity> dataList) {
    double ma = 0;
    for (int i = 0; i < dataList.length; i++) {
      KLineEntity entity = dataList[i];
      ma += entity.close;
      if (i == day - 1) {
        entity.BOLLMA = ma / day;
      } else if (i >= day) {
        ma -= dataList[i - day].close;
        entity.BOLLMA = ma / day;
      } else {
        entity.BOLLMA = null;
      }
    }
  }

  static void calcMACD(List<KLineEntity> dataList) {
    double ema12 = 0;
    double ema26 = 0;
    double dif = 0;
    double dea = 0;
    double macd = 0;

    for (int i = 0; i < dataList.length; i++) {
      KLineEntity entity = dataList[i];
      final closePrice = entity.close;
      if (i == 0) {
        ema12 = closePrice;
        ema26 = closePrice;
      } else {
        // EMA（12） = 前一日EMA（12） X 11/13 + 今日收盘价 X 2/13
        ema12 = ema12 * 11 / 13 + closePrice * 2 / 13;
        // EMA（26） = 前一日EMA（26） X 25/27 + 今日收盘价 X 2/27
        ema26 = ema26 * 25 / 27 + closePrice * 2 / 27;
      }
      // DIF = EMA（12） - EMA（26） 。
      // 今日DEA = （前一日DEA X 8/10 + 今日DIF X 2/10）
      // 用（DIF-DEA）*2即为MACD柱状图。
      dif = ema12 - ema26;
      dea = dea * 8 / 10 + dif * 2 / 10;
      macd = (dif - dea) * 2;
      entity.dif = dif;
      entity.dea = dea;
      entity.macd = macd;
    }
  }

  static void calcVolumeMA(List<KLineEntity> dataList) {
    double volumeMa5 = 0;
    double volumeMa10 = 0;

    for (int i = 0; i < dataList.length; i++) {
      KLineEntity entry = dataList[i];

      volumeMa5 += entry.vol;
      volumeMa10 += entry.vol;

      if (i == 4) {
        entry.MA5Volume = (volumeMa5 / 5);
      } else if (i > 4) {
        volumeMa5 -= dataList[i - 5].vol;
        entry.MA5Volume = volumeMa5 / 5;
      } else {
        entry.MA5Volume = 0;
      }

      if (i == 9) {
        entry.MA10Volume = volumeMa10 / 10;
      } else if (i > 9) {
        volumeMa10 -= dataList[i - 10].vol;
        entry.MA10Volume = volumeMa10 / 10;
      } else {
        entry.MA10Volume = 0;
      }
    }
  }

  static void calcRSI(List<KLineEntity> dataList) {
    double? rsi;
    double rsiABSEma = 0;
    double rsiMaxEma = 0;
    for (int i = 0; i < dataList.length; i++) {
      KLineEntity entity = dataList[i];
      final double closePrice = entity.close;
      if (i == 0) {
        rsi = 0;
        rsiABSEma = 0;
        rsiMaxEma = 0;
      } else {
        double rMax = max(0, closePrice - dataList[i - 1].close.toDouble());
        double rAbs = (closePrice - dataList[i - 1].close.toDouble()).abs();

        rsiMaxEma = (rMax + (14 - 1) * rsiMaxEma) / 14;
        rsiABSEma = (rAbs + (14 - 1) * rsiABSEma) / 14;
        rsi = (rsiMaxEma / rsiABSEma) * 100;
      }
      if (i < 13) rsi = null;
      if (rsi != null && rsi.isNaN) rsi = null;
      entity.rsi = rsi;
    }
  }

  static void calcKDJ(List<KLineEntity> dataList) {
    var preK = 50.0;
    var preD = 50.0;
    final tmp = dataList.first;
    tmp.k = preK;
    tmp.d = preD;
    tmp.j = 50.0;
    for (int i = 1; i < dataList.length; i++) {
      final entity = dataList[i];
      final n = max(0, i - 8);
      var low = entity.low;
      var high = entity.high;
      for (int j = n; j < i; j++) {
        final t = dataList[j];
        if (t.low < low) {
          low = t.low;
        }
        if (t.high > high) {
          high = t.high;
        }
      }
      final cur = entity.close;
      var rsv = (cur - low) * 100.0 / (high - low);
      rsv = rsv.isNaN ? 0 : rsv;
      final k = (2 * preK + rsv) / 3.0;
      final d = (2 * preD + k) / 3.0;
      final j = 3 * k - 2 * d;
      preK = k;
      preD = d;
      entity.k = k;
      entity.d = d;
      entity.j = j;
    }
  }

  static void calcWR(List<KLineEntity> dataList) {
    double r;
    for (int i = 0; i < dataList.length; i++) {
      KLineEntity entity = dataList[i];
      int startIndex = i - 14;
      if (startIndex < 0) {
        startIndex = 0;
      }
      double max14 = double.minPositive;
      double min14 = double.maxFinite;
      for (int index = startIndex; index <= i; index++) {
        max14 = max(max14, dataList[index].high);
        min14 = min(min14, dataList[index].low);
      }
      if (i < 13) {
        entity.r = -10;
      } else {
        r = -100 * (max14 - dataList[i].close) / (max14 - min14);
        if (r.isNaN) {
          entity.r = null;
        } else {
          entity.r = r;
        }
      }
    }
  }

  static void calcCCI(List<KLineEntity> dataList) {
    final size = dataList.length;
    final count = 14;
    for (int i = 0; i < size; i++) {
      final kline = dataList[i];
      final tp = (kline.high + kline.low + kline.close) / 3;
      final start = max(0, i - count + 1);
      var amount = 0.0;
      var len = 0;
      for (int n = start; n <= i; n++) {
        amount += (dataList[n].high + dataList[n].low + dataList[n].close) / 3;
        len++;
      }
      final ma = amount / len;
      amount = 0.0;
      for (int n = start; n <= i; n++) {
        amount +=
            (ma - (dataList[n].high + dataList[n].low + dataList[n].close) / 3)
                .abs();
      }
      final md = amount / len;
      kline.cci = ((tp - ma) / 0.015 / md);
      if (kline.cci!.isNaN) {
        kline.cci = 0.0;
      }
    }
  }

  // === DataUtil.dart 內新增：TradingView 版 DMI/ADX ===
  static void calculateDMI(List<KLineEntity> data, {int len = 14, int lensig = 14}) {
    final n = data.length;
    if (n < 2) return;

    // up = change(high), down = -change(low)
    final plusDM  = List<double>.filled(n, 0);
    final minusDM = List<double>.filled(n, 0);
    final tr      = List<double>.filled(n, 0);

    for (int i = 1; i < n; i++) {
      final up   = data[i].high - data[i - 1].high;
      final down = data[i - 1].low - data[i].low; // 已含負號

      plusDM[i]  = (up > down && up > 0)   ? up   : 0.0; // Pine: up > down and up > 0 ? up : 0
      minusDM[i] = (down > up && down > 0) ? down : 0.0; // Pine: down > up and down > 0 ? down : 0

      final tr1 = data[i].high - data[i].low;
      final tr2 = (data[i].high - data[i - 1].close).abs();
      final tr3 = (data[i].low  - data[i - 1].close).abs();
      tr[i] = tr1 > tr2 ? (tr1 > tr3 ? tr1 : tr3) : (tr2 > tr3 ? tr2 : tr3); // max(tr1,tr2,tr3)
    }

    // ta.rma 等同 Wilder 平滑： seed=SMA(len)，之後 rma = (prev*(len-1)+x)/len
    List<double?> _rma(List<double> src, int l) {
      final out = List<double?>.filled(src.length, null);
      if (l <= 1) {
        for (int i = 0; i < src.length; i++) out[i] = src[i];
        return out;
      }
      if (src.length < l) return out;

      double sum = 0;
      for (int i = 0; i < l; i++) sum += src[i];
      double prev = sum / l;
      out[l - 1] = prev;

      for (int i = l; i < src.length; i++) {
        prev = (prev * (l - 1) + src[i]) / l;
        out[i] = prev;
      }
      return out;
    }

    final trRma    = _rma(tr, len);         // trur = ta.rma(ta.tr, len)
    final pdmRma   = _rma(plusDM, len);     // ta.rma(plusDM, len)
    final mdmRma   = _rma(minusDM, len);    // ta.rma(minusDM, len)

    final plus  = List<double?>.filled(n, null);
    final minus = List<double?>.filled(n, null);

    for (int i = 0; i < n; i++) {
      final trv = trRma[i];
      final pr  = pdmRma[i];
      final mr  = mdmRma[i];
      if (trv != null && trv != 0 && pr != null && mr != null) {
        // fixnan(100 * rma(dm,len) / trur)；我們用 null 表示前置不足，不畫線更乾淨
        plus[i]  = 100.0 * pr / trv;
        minus[i] = 100.0 * mr / trv;
      }
    }

    // adx = 100 * ta.rma(|plus - minus| / max(sum,1), lensig)
    final dxInput = List<double>.filled(n, 0);
    for (int i = 0; i < n; i++) {
      final p = plus[i];
      final m = minus[i];
      if (p != null && m != null) {
        final s = (p + m).abs();
        dxInput[i] = (s == 0) ? 0 : ((p - m).abs() / s);
      } // else 0，讓 RMA 在有效區間後逐步收斂
    }
    final adxRma = _rma(dxInput, lensig);
    final adx    = List<double?>.filled(n, null);
    for (int i = 0; i < n; i++) {
      adx[i] = adxRma[i] == null ? null : 100.0 * adxRma[i]!;
    }

    // 回灌到 entity（把欄位加在 MACDEntity 或 KLineEntity；之前我們建議加在 MACDEntity）
    for (int i = 0; i < n; i++) {
      data[i].pdi = plus[i];
      data[i].mdi = minus[i];
      data[i].adx = adx[i];
      // 若要 ADXR，可再算：adxr[i] = (adx[i] + adx[i-len]) / 2
    }
  }
}
