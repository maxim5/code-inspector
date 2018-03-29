{ *******************************************************************************
  *                                                                             *
  *          Delphi Standard Template Library                                   *
  *                                                                             *
  *          (C)Copyright Jimx 2011                                             *
  *                                                                             *
  *          http://delphi-standard-template-library.googlecode.com             *
  *                                                                             *
  *******************************************************************************
  *  This file is part of Delphi Standard Template Library.                     *
  *                                                                             *
  *  Delphi Standard Template Library is free software:                         *
  *  you can redistribute it and/or modify                                      *
  *  it under the terms of the GNU General Public License as published by       *
  *  the Free Software Foundation, either version 3 of the License, or          *
  *  (at your option) any later version.                                        *
  *                                                                             *
  *  Delphi Standard Template Library is distributed                            *
  *  in the hope that it will be useful,                                        *
  *  but WITHOUT ANY WARRANTY; without even the implied warranty of             *
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *
  *  GNU General Public License for more details.                               *
  *                                                                             *
  *  You should have received a copy of the GNU General Public License          *
  *  along with Delphi Standard Template Library.                               *
  *  If not, see <http://www.gnu.org/licenses/>.                                *
  ******************************************************************************* }
unit DSTL.Algorithm;

interface

uses Generics.Collections, Generics.Defaults, DSTL.STL.Iterator;

type

{$REGION 'TIterAlgorithms'}
  Func<T> = function(p: T): T;

  TIterAlgorithms<T> = class
    procedure for_each(first, last: TIterator<T>; f: Func<T>);
  end;

  Func<T1, T2> = procedure(p: TPair<T1, T2>);

  TIterAlgorithms<T1, T2> = class
    procedure for_each(first, last: TIterator<T1, T2>; f: Func<T1, T2>);
  end;
{$ENDREGION}

{$REGION 'TMinMax<T>'}

  TMinMax<T> = class
    function min(data: array of T): T;
    function max(data: array of T): T;
  end;

{$ENDREGION}

{$REGION 'String Algorithm'}

function to_upper(s: string): string;
function to_lower(s: string): string;

function is_alpha(c: char): boolean;
function is_digit(c: char): boolean;
function is_alnum(c: char): boolean;
function is_lower(c: char): boolean;
function is_upper(c: char): boolean;

{$ENDREGION}

implementation

{$REGION 'TIterAlgorithms'}

procedure TIterAlgorithms<T>.for_each(first, last: TIterator<T>; f: Func<T>);
var
  op: TIterOperations<T>;
begin
  while not(op.equals(first, last)) do
  begin
    f(first.handle.iget(first));
    op.advance(first);
  end;
end;

procedure TIterAlgorithms<T1, T2>.for_each(first, last: TIterator<T1, T2>;
  f: Func<T1, T2>);
begin
  { TODO: for_each for TIterAlgorithm<T1, T2> }
end;
{$ENDREGION}

{$REGION 'TMinMax<T>'}

function TMinMax<T>.min(data: array of T): T;
var
  i: integer;
  tmp: T;
  comparer: IComparer<T>;
begin
  comparer := TComparer<T>.Default;
  tmp := data[low(data)];
  for i := low(data) + 1 to high(data) do
    if comparer.Compare(data[i], tmp) < 0 then
      tmp := data[i];
  result := tmp;
end;

function TMinMax<T>.max(data: array of T): T;
var
  i: integer;
  tmp: T;
  comparer: IComparer<T>;
begin
  comparer := TComparer<T>.Default;
  tmp := data[low(data)];
  for i := low(data) + 1 to high(data) do
    if comparer.Compare(data[i], tmp) > 0 then
      tmp := data[i];
  result := tmp;
end;

{$ENDREGION}

{$REGION 'String Algorithm'}

function upcase(c: char): char;
begin
  if (c >= 'a') and (c <= 'z') then
    result := chr(ord(c) + (ord('A') - ord('a')))
  else
    result := c;
end;

function lowcase(c: char): char;
begin
  if (c >= 'A') and (c <= 'Z') then
    result := chr(ord(c) - (ord('A') - ord('a')))
  else
    result := c;
end;

function to_upper(s: string): string;
var
  i: integer;
  r: string;
begin
  r := '';
  for i := 1 to length(s) do
    r := r + upcase(s[i]);
  result := r;
end;

function to_lower(s: string): string;
var
  i: integer;
  r: string;
begin
  r := '';
  for i := 1 to length(s) do
    r := r + lowcase(s[i]);
  result := r;
end;

function is_alpha(c: char): boolean;
begin
  result := ((c >= 'a') and (c <= 'z')) or ((c >= 'A') and (c <= 'Z'));
end;

function is_digit(c: char): boolean;
begin
  result := ((c >= '0') and (c <= '9'));
end;

function is_alnum(c: char): boolean;
begin
  result := is_alpha(c) or is_digit(c);
end;

function is_lower(c: char): boolean;
begin
  result := ((c >= 'a') and (c <= 'z'));
end;

function is_upper(c: char): boolean;
begin
  result := ((c >= 'A') and (c <= 'Z'));
end;

{$ENDREGION}

end.
