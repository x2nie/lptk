{ unitkeys.pas: some unicode translations
  File maintainer: nvitya@freemail.hu

History:
}

unit unitkeys;

{$ifdef FPC}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils; 
  
function TranslateChar(page, ch : char) : char;
  
// This conversion was taken from the OppenOffice 1.0.1 source code.
procedure KeySymToUnicode(ks : longword; res : PWord);

implementation

type
  unichar = word;

function TranslateChar(page, ch : char) : char;
begin
  result := ch;
  // some simple hungarian translation>
  if ord(page) = 1 then
  begin
    case ord(ch) of
      $50 : result := chr($D5);
      $51 : result := chr($F5);

      $70 : result := chr($DB);
      $71 : result := chr($FB);
    end;
    //writeln(ord(ch),' -> ',result);
  end;
end;
  
const

// Latin-1 		Byte 3 = $00
  keymap00 : array[32..255] of unichar = (
    $0020, $0021, $0022, $0023, $0024, $0025, $0026, $0027,
    $0028, $0029, $002a, $002b, $002c, $002d, $002e, $002f,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
    $0038, $0039, $003a, $003b, $003c, $003d, $003e, $003f,
    $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047,
    $0048, $0049, $004a, $004b, $004c, $004d, $004e, $004f,
    $0050, $0051, $0052, $0053, $0054, $0055, $0056, $0057,
    $0058, $0059, $005a, $005b, $005c, $005d, $005e, $005f,
    $0060, $0061, $0062, $0063, $0064, $0065, $0066, $0067,
    $0068, $0069, $006a, $006b, $006c, $006d, $006e, $006f,
    $0070, $0071, $0072, $0073, $0074, $0075, $0076, $0077,
    $0078, $0079, $007a, $007b, $007c, $007d, $007e, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $00a0, $00a1, $00a2, $00a3, $00a4, $00a5, $00a6, $00a7,
    $00a8, $00a9, $00aa, $00ab, $00ac, $00ad, $00ae, $00af,
    $00b0, $00b1, $00b2, $00b3, $00b4, $00b5, $00b6, $00b7,
    $00b8, $00b9, $00ba, $00bb, $00bc, $00bd, $00be, $00bf,
    $00c0, $00c1, $00c2, $00c3, $00c4, $00c5, $00c6, $00c7,
    $00c8, $00c9, $00ca, $00cb, $00cc, $00cd, $00ce, $00cf,
    $00d0, $00d1, $00d2, $00d3, $00d4, $00d5, $00d6, $00d7,
    $00d8, $00d9, $00da, $00db, $00dc, $00dd, $00de, $00df,
    $00e0, $00e1, $00e2, $00e3, $00e4, $00e5, $00e6, $00e7,
    $00e8, $00e9, $00ea, $00eb, $00ec, $00ed, $00ee, $00ef,
    $00f0, $00f1, $00f2, $00f3, $00f4, $00f5, $00f6, $00f7,
    $00f8, $00f9, $00fa, $00fb, $00fc, $00fd, $00fe, $00ff
                                    );
                                    
// Latin-2		Byte 3 = $01
  keymap01 : array[161..255] of unichar = (
    $0104, $02d8, $0141, $0000, $013d, $015a, $0000, $0000,
    $0160, $015e, $0164, $0179, $0000, $017d, $017b, $0000,
    $0105, $02db, $0142, $0000, $013e, $015b, $02c7, $0000,
    $0161, $015f, $0165, $017a, $02dd, $017e, $017c, $0154,
    $0000, $0000, $0102, $0000, $0139, $0106, $0000, $010c,
    $0000, $0118, $0000, $011a, $0000, $0000, $010e, $0110,
    $0143, $0147, $0000, $0000, $0150, $0000, $0000, $0158,
    $016e, $0000, $0170, $0000, $0000, $0162, $0000, $0155,
    $0000, $0000, $0103, $0000, $013a, $0107, $0000, $010d,
    $0000, $0119, $0000, $011b, $0000, $0000, $010f, $0111,
    $0144, $0148, $0000, $0000, $0151, $0000, $0000, $0159,
    $016f, $0000, $0171, $0000, $0000, $0163, $02d9 );
    
// Latin-3		Byte 3 = $02
  keymap02 : array[161..254] of unichar = (
    $0126, $0000, $0000, $0000, $0000, $0124, $0000, $0000,
    $0130, $0000, $011e, $0134, $0000, $0000, $0000, $0000,
    $0127, $0000, $0000, $0000, $0000, $0125, $0000, $0000,
    $0131, $0000, $011f, $0135, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $010a, $0108, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0120, $0000, $0000, $011c,
    $0000, $0000, $0000, $0000, $016c, $015c, $0000, $0000,
    $0000, $0000, $0000, $0000, $010b, $0109, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0121, $0000, $0000, $011d,
    $0000, $0000, $0000, $0000, $016d, $015d );

// Latin-4		Byte 3 = $03
  keymap03 : array[162..254] of unichar = (
    $0138, $0156, $0000, $0128, $013b, $0000, $0000, $0000,
    $0112, $0122, $0166, $0000, $0000, $0000, $0000, $0000,
    $0000, $0157, $0000, $0129, $013c, $0000, $0000, $0000,
    $0113, $0123, $0167, $014a, $0000, $014b, $0100, $0000,
    $0000, $0000, $0000, $0000, $0000, $012e, $0000, $0000,
    $0000, $0000, $0116, $0000, $0000, $012a, $0000, $0145,
    $014c, $0136, $0000, $0000, $0000, $0000, $0000, $0172,
    $0000, $0000, $0000, $0168, $016a, $0000, $0101, $0000,
    $0000, $0000, $0000, $0000, $0000, $012f, $0000, $0000,
    $0000, $0000, $0117, $0000, $0000, $012b, $0000, $0146,
    $014d, $0137, $0000, $0000, $0000, $0000, $0000, $0173,
    $0000, $0000, $0000, $0169, $016b );

// Kana			Byte 3 = $04
  keymap04 : array[126..223] of unichar = (
    $203e, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $3002, $300c, $300d, $3001, $30fb,
    $30f2, $30a1, $30a3, $30a5, $30a7, $30a9, $30e3, $30e5,
    $30e7, $30c3, $30fc, $30a2, $30a4, $30a6, $30a8, $30aa,
    $30ab, $30ad, $30af, $30b1, $30b3, $30b5, $30b7, $30b9,
    $30bb, $30bd, $30bf, $30c1, $30c4, $30c6, $30c8, $30ca,
    $30cb, $30cc, $30cd, $30ce, $30cf, $30d2, $30d5, $30d8,
    $30db, $30de, $30df, $30e0, $30e1, $30e2, $30e4, $30e6,
    $30e8, $30e9, $30ea, $30eb, $30ec, $30ed, $30ef, $30f3,
    $309b, $309c );

// Arabic		Byte 3 = $05
  keymap05 : array[172..242] of unichar = (
    $060c, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $061b,
    $0000, $0000, $0000, $061f, $0000, $0621, $0622, $0623,
    $0624, $0625, $0626, $0627, $0628, $0629, $062a, $062b,
    $062c, $062d, $062e, $062f, $0630, $0631, $0632, $0633,
    $0634, $0635, $0636, $0637, $0638, $0639, $063a, $0000,
    $0000, $0000, $0000, $0000, $0640, $0641, $0642, $0643,
    $0644, $0645, $0646, $0647, $0648, $0649, $064a, $064b,
    $064c, $064d, $064e, $064f, $0650, $0651, $0652 );

// Cyrillic		Byte 3 = $06
  keymap06 : array[161..255] of unichar = (
    $0452, $0453, $0451, $0454, $0455, $0456, $0457, $0458,
    $0459, $045a, $045b, $045c, $0000, $045e, $045f, $2116,
    $0402, $0403, $0401, $0404, $0405, $0406, $0407, $0408,
    $0409, $040a, $040b, $040c, $0000, $040e, $040f, $044e,
    $0430, $0431, $0446, $0434, $0435, $0444, $0433, $0445,
    $0438, $0439, $043a, $043b, $043c, $043d, $043e, $043f,
    $044f, $0440, $0441, $0442, $0443, $0436, $0432, $044c,
    $044b, $0437, $0448, $044d, $0449, $0447, $044a, $042e,
    $0410, $0411, $0426, $0414, $0415, $0424, $0413, $0425,
    $0418, $0419, $041a, $041b, $041c, $041d, $041e, $041f,
    $042f, $0420, $0421, $0422, $0423, $0416, $0412, $042c,
    $042b, $0417, $0428, $042d, $0429, $0427, $042a );

// Greek		Byte 3 = $07
  keymap07 : array[161..249] of unichar = (
    $0386, $0388, $0389, $038a, $03aa, $0000, $038c, $038e,
    $03ab, $0000, $038f, $0000, $0000, $0385, $2015, $0000,
    $03ac, $03ad, $03ae, $03af, $03ca, $0390, $03cc, $03cd,
    $03cb, $03b0, $03ce, $0000, $0000, $0000, $0000, $0000,
    $0391, $0392, $0393, $0394, $0395, $0396, $0397, $0398,
    $0399, $039a, $039b, $039c, $039d, $039e, $039f, $03a0,
    $03a1, $03a3, $0000, $03a4, $03a5, $03a6, $03a7, $03a8,
    $03a9, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $03b1, $03b2, $03b3, $03b4, $03b5, $03b6, $03b7, $03b8,
    $03b9, $03ba, $03bb, $03bc, $03bd, $03be, $03bf, $03c0,
    $03c1, $03c3, $03c2, $03c4, $03c5, $03c6, $03c7, $03c8,
    $03c9 );

// Technical	Byte 3 = $08
  keymap08 : array[161..254] of unichar = (
    $23b7, $250c, $2500, $2320, $2321, $2502, $23a1, $23a3,
    $23a4, $23a6, $239b, $239d, $239e, $23a0, $23a8, $23ac,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $2264, $2260, $2265, $222b, $2234,
    $221d, $221e, $0000, $0000, $2207, $0000, $0000, $223c,
    $2243, $0000, $0000, $0000, $21d4, $21d2, $2261, $0000,
    $0000, $0000, $0000, $0000, $0000, $221a, $0000, $0000,
    $0000, $2282, $2283, $2229, $222a, $2227, $2228, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $2202, $0000,
    $0000, $0000, $0000, $0000, $0000, $0192, $0000, $0000,
    $0000, $0000, $2190, $2191, $2192, $2193 );

// Special		Byte 3 = $09
  keymap09 : array[224..248] of unichar = (
    $25c6, $2592, $2409, $240c, $240d, $240a, $0000, $0000,
    $2424, $240b, $2518, $2510, $250c, $2514, $253c, $23ba,
    $23bb, $2500, $23bc, $23bd, $251c, $2524, $2534, $252c,
    $2502 );

// Publishing	Byte 3 = $0a = 10
  keymap10 : array[161..254] of unichar = (
    $2003, $2002, $2004, $2005, $2007, $2008, $2009, $200a,
    $2014, $2013, $0000, $0000, $0000, $2026, $2025, $2153,
    $2154, $2155, $2156, $2157, $2158, $2159, $215a, $2105,
    $0000, $0000, $2012, $2329, $0000, $232a, $0000, $0000,
    $0000, $0000, $215b, $215c, $215d, $215e, $0000, $0000,
    $2122, $2613, $0000, $25c1, $25b7, $25cb, $25af, $2018,
    $2019, $201c, $201d, $211e, $0000, $2032, $2033, $0000,
    $271d, $0000, $25ac, $25c0, $25b6, $25cf, $25ae, $25e6,
    $25ab, $25ad, $25b3, $25bd, $2606, $2022, $25aa, $25b2,
    $25bc, $261c, $261e, $2663, $2666, $2665, $0000, $2720,
    $2020, $2021, $2713, $2717, $266f, $266d, $2642, $2640,
    $260e, $2315, $2117, $2038, $201a, $201e );

// APL		Byte 3 = $0b = 11
  keymap11 : array[163..252] of unichar = (
    $003c, $0000, $0000, $003e, $0000, $2228, $2227, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $00af, $0000, $22a5,
    $2229, $230a, $0000, $005f, $0000, $0000, $0000, $2218,
    $0000, $2395, $0000, $22a4, $25cb, $0000, $0000, $0000,
    $2308, $0000, $0000, $222a, $0000, $2283, $0000, $2282,
    $0000, $22a2, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $22a3 );

// Hebrew	Byte 3 = $0c = 12
  keymap12 : array[223..250] of unichar = (
    $2017, $05d0, $05d1, $05d2, $05d3, $05d4, $05d5, $05d6,
    $05d7, $05d8, $05d9, $05da, $05db, $05dc, $05dd, $05de,
    $05df, $05e0, $05e1, $05e2, $05e3, $05e4, $05e5, $05e6,
    $05e7, $05e8, $05e9, $05ea );

// Thai		Byte 3 = $0d = 13
  keymap13 : array[161..249] of unichar = (
    $0e01, $0e02, $0e03, $0e04, $0e05, $0e06, $0e07, $0e08,
    $0e09, $0e0a, $0e0b, $0e0c, $0e0d, $0e0e, $0e0f, $0e10,
    $0e11, $0e12, $0e13, $0e14, $0e15, $0e16, $0e17, $0e18,
    $0e19, $0e1a, $0e1b, $0e1c, $0e1d, $0e1e, $0e1f, $0e20,
    $0e21, $0e22, $0e23, $0e24, $0e25, $0e26, $0e27, $0e28,
    $0e29, $0e2a, $0e2b, $0e2c, $0e2d, $0e2e, $0e2f, $0e30,
    $0e31, $0e32, $0e33, $0e34, $0e35, $0e36, $0e37, $0e38,
    $0e39, $0e3a, $0000, $0000, $0000, $0000, $0e3f, $0e40,
    $0e41, $0e42, $0e43, $0e44, $0e45, $0e46, $0e47, $0e48,
    $0e49, $0e4a, $0e4b, $0e4c, $0e4d, $0000, $0000, $0e50,
    $0e51, $0e52, $0e53, $0e54, $0e55, $0e56, $0e57, $0e58,
    $0e59 );

// Korean		Byte 3 = $0e = 14
  keymap14 : array[161..255] of unichar = (
    $3131, $3132, $3133, $3134, $3135, $3136, $3137, $3138,
    $3139, $313a, $313b, $313c, $313d, $313e, $313f, $3140,
    $3141, $3142, $3143, $3144, $3145, $3146, $3147, $3148,
    $3149, $314a, $314b, $314c, $314d, $314e, $314f, $3150,
    $3151, $3152, $3153, $3154, $3155, $3156, $3157, $3158,
    $3159, $315a, $315b, $315c, $315d, $315e, $315f, $3160,
    $3161, $3162, $3163, $11a8, $11a9, $11aa, $11ab, $11ac,
    $11ad, $11ae, $11af, $11b0, $11b1, $11b2, $11b3, $11b4,
    $11b5, $11b6, $11b7, $11b8, $11b9, $11ba, $11bb, $11bc,
    $11bd, $11be, $11bf, $11c0, $11c1, $11c2, $316d, $3171,
    $3178, $317f, $3181, $3184, $3186, $318d, $318e, $11eb,
    $11f0, $11f9, $0000, $0000, $0000, $0000, $20a9 );

// missing:
// Latin-8  	Byte 3 = $12 = 18

// Latin-9 		Byte 3 = $13 = 19
  keymap19 : array[188..190] of unichar = (
    $0152, $0153, $0178 );

// missing:
// Armenian 	Byte 3 = $14 = 20
// Georgian 	Byte 3 = $15 = 21
// Azeri 		Byte 3 = $16 = 22
// Vietnamese 	Byte 3 = $1e = 30

// Currency		Byte 3 = $20 = 32
  keymap32 : array[160..172] of unichar = (
    $20a0, $20a1, $20a2, $20a3, $20a4, $20a5, $20a6, $20a7,
    $20a8, $0000, $20aa, $20ab, $20ac );

// Keyboard (Keypad mappings) Byte 3 = $ff = 255
  keymap255 : array[128..189] of unichar = (
    $0020, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $000d, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $0000, $0000, $0000, $0000, $0000, $0000,
    $0000, $0000, $002a, $002b, $002c, $002d, $002e, $002f,
    $0030, $0031, $0032, $0033, $0034, $0035, $0036, $0037,
    $0038, $0039, $0000, $0000, $0000, $003d );

type
  keymap = array[0..255] of word;
  mapptr = ^keymap;

procedure TranslateCode(var res : word; ccode : byte; map : mapptr; lowind,highind : byte);
begin
  //writeln('code: ',ccode,' low: ',lowind,' high: ',highind);
  if (ccode >= lowind) and (ccode <= highind) then res := map^[ccode-lowind];
end;

procedure KeySymToUnicode(ks : longword; res : PWord);
var
  table : byte;
  ccode : byte;
begin
  table := ((ks and $0000FF00) shr 8);
  ccode := ks and $ff;
  res^ := ks and $FFFF;
  case table of
    00 : TranslateCode(res^, ccode, @keymap00, low(keymap00), high(keymap00) );
    01 : TranslateCode(res^, ccode, @keymap01, low(keymap01), high(keymap01) );
    02 : TranslateCode(res^, ccode, @keymap02, low(keymap01), high(keymap02) );
    03 : TranslateCode(res^, ccode, @keymap03, low(keymap01), high(keymap03) );
    04 : TranslateCode(res^, ccode, @keymap04, low(keymap01), high(keymap04) );
    05 : TranslateCode(res^, ccode, @keymap05, low(keymap01), high(keymap05) );
    06 : TranslateCode(res^, ccode, @keymap06, low(keymap01), high(keymap06) );
    07 : TranslateCode(res^, ccode, @keymap07, low(keymap01), high(keymap07) );
    08 : TranslateCode(res^, ccode, @keymap08, low(keymap01), high(keymap08) );
    09 : TranslateCode(res^, ccode, @keymap09, low(keymap01), high(keymap09) );
    
    10 : TranslateCode(res^, ccode, @keymap10, low(keymap10), high(keymap10) );
    11 : TranslateCode(res^, ccode, @keymap11, low(keymap11), high(keymap11) );
    12 : TranslateCode(res^, ccode, @keymap12, low(keymap12), high(keymap12) );
    13 : TranslateCode(res^, ccode, @keymap13, low(keymap13), high(keymap13) );
    14 : TranslateCode(res^, ccode, @keymap14, low(keymap14), high(keymap14) );
//    15 : TranslateCode(res^, ccode, @keymap15, low(keymap15), high(keymap15) );
//    16 : TranslateCode(res^, ccode, @keymap16, low(keymap16), high(keymap16) );
//    17 : TranslateCode(res^, ccode, @keymap17, low(keymap17), high(keymap17) );
//    18 : TranslateCode(res^, ccode, @keymap18, low(keymap18), high(keymap18) );
    19 : TranslateCode(res^, ccode, @keymap19, low(keymap19), high(keymap19) );
    
    32 : TranslateCode(res^, ccode, @keymap32, low(keymap32), high(keymap32) );
    
    255 : TranslateCode(res^, ccode, @keymap255, low(keymap255), high(keymap255) );
  end;
end;
                                    
end.
