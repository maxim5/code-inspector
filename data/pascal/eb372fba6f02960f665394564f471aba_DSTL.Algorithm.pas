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

uses Generics.Defaults, DSTL.STL.Iterator, DSTL.Types, DSTL.Utils.Pair;

type

{$REGION 'TIterAlgorithms'}
  Func<T> = function(p: T): T;

  TIterAlgorithms<T> = class
    (* we cannot do this yet
     * class function accumulate(first, last: TIterator<T>; init: T);
     *)
    class function accumulate(first, last: TIterator<T>; init: T; binary_op:
                                                TBinaryFunction<T, T, T>): T;
    (* class function adjacent_difference(first, last: TIterator<T>; res: TIterator<T>;
                        ): TIterator<T>; *)
    class function adjacent_difference(first, last: TIterator<T>; res: TIterator<T>;
                        binary_op: TBinaryFunction<T, T, T>): TIterator<T>;
    class function adjacent_find(first, last: TIterator<T>): TIterator<T>;  overload;
    class function adjacent_find(first, last: TIterator<T>; pred: TBinaryPredicate<T, T>): TIterator<T>;  overload;
    class function copy(first, last, res: TIterator<T>): TIterator<T>;
    class function copy_backward(first, last, res: TIterator<T>): TIterator<T>;
    class function count(first, last: TIterator<T>; value: T): integer;
    class function count_if(first, last: TIterator<T>; pred: TPredicate<T>): integer;
    class procedure fill(first, last: TIterator<T>; value: T);
    class function fill_n(first: TIterator<T>; n: integer; value: T): TIterator<T>;
    class function find(first, last: TIterator<T>; value: T): TIterator<T>;
    class function find_if(first, last: TIterator<T>; pred: TPredicate<T>): TIterator<T>;
    class function find_end(first1, last1, first2, last2: TIterator<T>): TIterator<T>;  overload;
    class function find_end(first1, last1, first2, last2: TIterator<T>; pred: TBinaryPredicate<T, T>): TIterator<T>; overload;
    class function find_first_of(first1, last1, first2, last2: TIterator<T>): TIterator<T>;  overload;
    class function find_first_of(first1, last1, first2, last2: TIterator<T>; pred: TBinaryPredicate<T, T>): TIterator<T>; overload;
    class procedure for_each(first, last: TIterator<T>; f: Func<T>);
  end;

  Func<T1, T2> = procedure(p: TPair<T1, T2>);

  TIterAlgorithms<T1, T2> = class
    class procedure for_each(first, last: TIterator<T1, T2>; f: Func<T1, T2>);
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

class function TIterAlgorithms<T>.accumulate(first, last: TIterator<T>; init: T;
                                binary_op: TBinaryFunction<T, T, T>): T;
var
  it: TIterator<T>;
begin
  Result := init;
  it := first;

  while it <> last do
  begin
    Result := binary_op(Result, it.handle.iget(it));
  end;
end;

class function TIterAlgorithms<T>.adjacent_difference(first, last: TIterator<T>;
         res: TIterator<T>; binary_op: TBinaryFunction<T, T, T>): TIterator<T>;
var
  it, it2: TIterator<T>;
begin
  if first.handle.iequals(first, last) then exit(res);

  it := first;
  res.handle.iput(res, it);
  it.handle.iadvance(it);
  res.handle.iadvance(res);
  while it <> last do
  begin
    it2 := it; it2.handle.iretreat(it2);
    res.handle.iput(res, binary_op(it.handle.iget(it), it2.handle.iget(it2)));
    it.handle.iadvance(it);
    res.handle.iadvance(res);
  end;
end;

class function TIterAlgorithms<T>.adjacent_find(first, last: TIterator<T>): TIterator<T>;
var
  next: TIterator<T>;
  comp: IComparer<T>;
begin
  comp := TComparer<T>.Default;
  if not first.handle.iequals(first, last) then
  begin
    next := first;
    next.handle.iadvance(next);
    while not next.handle.iequals(next, last) do
    begin
      if comp.Compare(first.handle.iget(first), next.handle.iget(next)) = 0then exit(first)
      else
      begin
        next.handle.iadvance(next);
        first.handle.iadvance(first);
      end;
    end;
  end;
  Result := last;
end;

class function TIterAlgorithms<T>.adjacent_find(first, last: TIterator<T>; pred: TBinaryPredicate<T, T>): TIterator<T>;
var
  next: TIterator<T>;
begin
  if not first.handle.iequals(first, last) then
  begin
    next := first;
    next.handle.iadvance(next);
    while not next.handle.iequals(next, last) do
    begin
      if pred(first.handle.iget(first), next.handle.iget(next)) then exit(first)
      else
      begin
        next.handle.iadvance(next);
        first.handle.iadvance(first);
      end;
    end;
  end;
  Result := last;
end;

class function TIterAlgorithms<T>.copy(first, last, res: TIterator<T>): TIterator<T>;
var
  n: integer;
begin
  n := first.handle.idistance(first, last);
  while n > 0 do
  begin
    res.handle.iput(res, first.handle.iget(first));
    first.handle.iadvance(first);
    res.handle.iadvance(res);
    dec(n);
  end;
  Result := res;
end;

class function TIterAlgorithms<T>.copy_backward(first, last, res: TIterator<T>): TIterator<T>;
begin
  while not first.handle.iequals(first, last) do
  begin
    first.handle.iretreat(last);
    res.handle.iretreat(res);
    res.handle.iput(res, last.handle.iget(last));
  end;
  Result := res;
end;

class function TIterAlgorithms<T>.count(first, last: TIterator<T>; value: T): integer;
begin
  Result := 0;
  while not first.handle.iequals(first, last) do
  begin
    if TComparer<T>.Default.Compare(first.handle.iget(first), value) = 0 then inc(Result);
    first.handle.iadvance(first);
  end;
end;

class function TIterAlgorithms<T>.count_if(first, last: TIterator<T>; pred: TPredicate<T>): integer;
begin
  Result := 0;
  while not first.handle.iequals(first, last) do
  begin
    if pred(first.handle.iget(first)) then inc(Result);
    first.handle.iadvance(first);
  end;
end;

class procedure TIterAlgorithms<T>.fill(first, last: TIterator<T>; value: T);
begin
  while not first.handle.iequals(first, last) do
  begin
    first.handle.iput(first, value);
    first.handle.iadvance(first);
  end;
end;

class function TIterAlgorithms<T>.fill_n(first: TIterator<T>; n: integer; value: T): TIterator<T>;
begin
  while n > 0 do
  begin
    first.handle.iput(first, value);
    first.handle.iadvance(first);
    dec(n);
  end;
  Result := first;
end;

class function TIterAlgorithms<T>.find(first, last: TIterator<T>; value: T): TIterator<T>;
var
  comp: IComparer<T>;
begin
  comp := TComparer<T>.Default;
  while not first.handle.iequals(first, last) do
  begin
    if comp.Compare(first.handle.iget(first), value) = 0 then break;
    first.handle.iadvance(first);
  end;
  Result := first;
end;

class function TIterAlgorithms<T>.find_if(first, last: TIterator<T>; pred: TPredicate<T>): TIterator<T>;
begin
  while not first.handle.iequals(first, last) do
  begin
    if pred(first.handle.iget(first)) then break;
    first.handle.iadvance(first);
  end;
  Result := first;
end;

class function TIterAlgorithms<T>.find_end(first1, last1, first2, last2: TIterator<T>): TIterator<T>;
var
  ret, it1, it2: TIterator<T>;
  comp: IComparer<T>;
begin
  comp := TComparer<T>.Default;

  if first2.handle.iequals(first2, last2) then exit(last1);

  ret := last1;

  while not first1.handle.iequals(first1, last1) do
  begin
    it1 := first1;
    it2 := first2;
    while comp.Compare(it1.handle.iget(it1), it2.handle.iget(it2)) = 0 do
    begin
      inc(it1);
      inc(it2);
      if it2.handle.iequals(it2, last2) then
      begin
        ret := first1;
        break;
      end;
      if it1.handle.iequals(it1, last1) then exit(ret);
    end;
    first1.handle.iadvance(first1);
  end;

  Result :=  ret;
end;

class function TIterAlgorithms<T>.find_end(first1, last1, first2, last2: TIterator<T>;
                                     pred: TBinaryPredicate<T, T>): TIterator<T>;
var
  ret, it1, it2: TIterator<T>;
begin
  if first2.handle.iequals(first2, last2) then exit(last1);

  ret := last1;

  while not first1.handle.iequals(first1, last1) do
  begin
    it1 := first1;
    it2 := first2;
    while pred(it1.handle.iget(it1), it2.handle.iget(it2)) do
    begin
      inc(it1);
      inc(it2);
      if it2.handle.iequals(it2, last2) then
      begin
        ret := first1;
        break;
      end;
      if it1.handle.iequals(it1, last1) then exit(ret);
    end;
    first1.handle.iadvance(first1);
  end;

  Result :=  ret;
end;

class function TIterAlgorithms<T>.find_first_of(first1, last1, first2, last2: TIterator<T>): TIterator<T>;
var
  it: TIterator<T>;
  comp: IComparer<T>;
begin
  comp := TComparer<T>.Default;
  while not first1.handle.iequals(first1, last1) do
  begin
    it := first2;
    while it.handle.iequals(it, last2) do
    begin
      if comp.Compare(it.handle.iget(it), last2.handle.iget(last2)) = 0 then exit(first1);
      it.handle.iadvance(it);
    end;
    first1.handle.iadvance(first1);
  end;
  Result := last1;
end;

class function TIterAlgorithms<T>.find_first_of(first1, last1, first2, last2: TIterator<T>;
                                     pred: TBinaryPredicate<T, T>): TIterator<T>;
var
  it: TIterator<T>;
begin
  while not first1.handle.iequals(first1, last1) do
  begin
    it := first2;
    while it.handle.iequals(it, last2) do
    begin
      if pred(it.handle.iget(it), last2.handle.iget(last2)) then exit(first1);
      it.handle.iadvance(it);
    end;
    first1.handle.iadvance(first1);
  end;
  Result := last1;
end;

class procedure TIterAlgorithms<T>.for_each(first, last: TIterator<T>; f: Func<T>);
var
  op: TIterOperations<T>;
begin
  while not(op.equals(first, last)) do
  begin
    f(first.handle.iget(first));
    op.advance(first);
  end;
end;

class procedure TIterAlgorithms<T1, T2>.for_each(first, last: TIterator<T1, T2>;
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
