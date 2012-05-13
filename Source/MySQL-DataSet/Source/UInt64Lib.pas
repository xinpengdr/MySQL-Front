unit UInt64Lib;
{
  Unsigned 64 bit Integer support for Delphi 6 and Kylix.
  Copyright (c) 2002 by DelphiFactory Netherlands BV

  This unit provides to routines to convert between strings and unsigned
  64 bit integers.

  UInt64 range: 0..18446744073709551615

  Thanks to J.H.Plantenberg for the assembler part.

  For comments mail to:
  erwin@delphi-factory.com
}

interface

resourcestring
  SInvalidUInt64 = '''%s'' is not a valid UInt64 value';

{ StrToUInt64 converts the given string to an unsigned 64 bit integer value.
  If the string doesn't contain a valid value, an EConvertError exception is
  raised. }
function StrToUInt64(const S: string): UInt64;

{ UInt64ToStr converts the given value to its decimal string representation. }
function UInt64ToStr(Value: UInt64): string;

implementation

// For speed we are going to use the fact that long strings are
// guaranteed to be null-terminated:
{$R-} // Range checking must be disabled
{$B-} // Do not perform complete boolean expression evaluations

uses
  SysUtils;

type
  _UInt64 = packed record   // Split the 64 bit into 2x32 bit
    Lo, Hi : LongWord;
  end;

procedure RaiseEConvertError(const S: string);
begin
  // raise an exception explaining the input string was invalid
  raise EConvertError.CreateResFmt(@SInvalidUInt64, [S]);
end;

function StrToUInt64(const S: string): UInt64;
var
  I: LongWord;
  Dig: Integer;
  Digit : LongWord;
  Hi,Lo : LongWord;

begin
  // check if S is empty (is nil pointer)
  if S = '' then
    RaiseEConvertError(S);

  // start at the first character
  I := 1;

  // trim leading spaces
  while S[I] = ' ' do
    Inc(I);

  if S[I] = '-' then // check for minus sign
    RaiseEConvertError(S);

  if S[I] = '+' then // check for plus sign
    Inc(I);

  // Check for hexidecimal string id:  '$' or '0x'
  if (S[I] = '$') or ((S[I] = '0') and (Upcase(S[I+1]) = 'X')) then
  begin
    // trim leading zero (if '0x' hex marker)
    if S[I] = '0' then
      Inc(I);

    // trim hex marker
    Inc(I);

    // Check if empty
    if S[I] = #0 then
      RaiseEConvertError(S);

    // init loop
    Dig := 0;
    Result := 0;

    // while not end of string
    while S[I] <> #0 do
    begin
      // try character convert
      case S[I] of
        '0'..'9': Dig := Ord(S[I]) -  Ord('0');
        'A'..'F': Dig := Ord(S[I]) - (Ord('A') - 10);
        'a'..'f': Dig := Ord(S[I]) - (Ord('a') - 10);
      else
        RaiseEConvertError(S)
      end;

      // still enough room in result?
      if Result shr 60 > 0 then
        RaiseEConvertError(S);

      // shift converted digit into result
      Result := Result shl 4 + Dig;

      // next char
      Inc(I);
    end;
  end
  else begin  // decimal unsigned 64 bit conversion

    // check if not empty
    if S[I] = #0 then
      RaiseEConvertError(S);

    Hi := 0;
    Lo := 0;
    while S[I] <> #0 do
    begin
      // Extract the digit from the string and convert it ASCII->Byte
      Digit := Ord(S[I]) xor Ord('0');

      // Some assembler to perform an unsigned 64 bit integer calculation.
      // This asm code runs in D6 and Kylix (PIC code).
      //  HiLo := (HiLo*10)+Digit
      asm
        push   esi               // save register

        // calculate: Hi * 10;
        mov    eax, Hi           // Load Hi
        mov    ecx, 10           // multiplier is 10
        mul    ecx               // EDX:EAX := EAX*ECX
        or     edx, edx          // Overflow?
        jnz    @TooBig           // yes -> bye bye

        // calculate: Lo * 10
        mov    esi, eax          // save Hi value
        mov    eax, Lo           // load Lo
        mul    ecx               // EDX:EAX := EAX*ECX

        // Combine Hi, Lo, and overflow of Lo to form HiLo result
        add    edx, esi          // EDX:EAX := HiLo*10

        // HiLo := HiLo + Digit
        add    eax, Digit        // EDX:EAX := HiLo+Digit
        adc    edx, 0            // check overflow
        jc     @TooBig           // yes -> bye bye

        // save HiLo
        mov    Hi, edx           // Hi := EDX
        mov    Lo, eax           // Lo := EAX
        jmp    @TheEnd           // successfull finish
      @TooBig:
        mov    Digit, 666        // something went wrong: invalidate Digit
      @TheEnd:

        pop    esi              // restore register
      end;

      // Check if digit was legal and if the previous calculation was a success
      if not (Digit in [0..9]) then
        RaiseEConvertError(S);

      // proceed to the next digit
      Inc(I);
    end;
    // Return HiLo as an unsigned 64 bit integer
    _UInt64(Result).Lo := Lo;
    _UInt64(Result).Hi := Hi;
  end;
end;


function UInt64ToStr(Value: UInt64): string;
const
  BiggestUInt64Str = '18446744073709551615';
  MaxBCD = Length(BiggestUInt64Str);

type
  TBCD = array[0..MaxBCD-1] of Integer; { Index 0 is highest BCD digit}

  procedure AddBCD(var BCD : TBCD; Pos,Value : Integer);
  begin
    Inc(BCD[Pos], Value);
    if BCD[Pos] > 9 then
    begin
      Dec(BCD[Pos],10);
      AddBCD(BCD,Pos-1, 1);
    end;
  end;

  procedure IncBCD(var A : TBCD; const B : TBCD);
  var
    I : Integer;
  begin
    for I := MaxBCD-1 downto 0 do
      AddBCD(A, I, B[I]);
  end;

var
  ValueBCD : TBCD;
  BitValue : TBCD;
  Tmp : TBCD;
  I : Integer;
  Ofs : Integer;

begin
  // default to zero
  FillChar(ValueBCD, SizeOf(ValueBCD), 0);

  // set bit value BCD. Lowest bit has value 1
  FillChar(BitValue, SizeOf(BitValue), 0);
  BitValue[MaxBCD-1] := 1;

  // check if there are bits available
  while Value <> 0 do
  begin
    // if current lowest bit is set
    //  Increment the BCD value with the current bit value
    if Value and 1 <> 0 then
      IncBCD(ValueBCD, BitValue);

    // proceed to the next bit
    Value := Value shr 1;

    // Double the BitValue
    Tmp := BitValue;
    IncBCD(BitValue, Tmp);
  end;

  // Find highest non zero decimal
  Ofs := 0;
  while (Ofs < MaxBCD) and (ValueBCD[Ofs] = 0) do
    Inc(Ofs);

  // check if any non zero decimals present
  if Ofs < MaxBCD then
  begin
    // convert BCD result to ASCII result
    SetLength(Result, MaxBCD-Ofs);
    I := Ofs;
    Repeat
      Result[I-Ofs+1] := Char(ValueBCD[I]+Ord('0'));
      Inc(I);
    until I > MaxBCD-1;
  end
  else
    Result := '0';  // nothing set -> value is '0'
end;

end.
