{***************************************************************************}
{                                                                           }
{           Spring Framework for Delphi                                     }
{                                                                           }
{           Copyright (c) 2009-2014 Spring4D Team                           }
{                                                                           }
{           http://www.spring4d.org                                         }
{                                                                           }
{***************************************************************************}
{                                                                           }
{  Licensed under the Apache License, Version 2.0 (the "License");          }
{  you may not use this file except in compliance with the License.         }
{  You may obtain a copy of the License at                                  }
{                                                                           }
{      http://www.apache.org/licenses/LICENSE-2.0                           }
{                                                                           }
{  Unless required by applicable law or agreed to in writing, software      }
{  distributed under the License is distributed on an "AS IS" BASIS,        }
{  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. }
{  See the License for the specific language governing permissions and      }
{  limitations under the License.                                           }
{                                                                           }
{***************************************************************************}


unit NullableType;

interface

uses sysutils, classes, rtti, variants, Generics.Defaults;

var
  SNullableTypeHasNoValue
    : string = 'Invalid operation, Nullable type has no value.';
  SCannotAssignPointerToNullable
    : string = 'Cannot assigned non-null pointer to nullable type.';

type

  ENotImplementedException = sysutils.ENotImplemented;
  EInvalidOperationException = sysutils.EInvalidOpException;
  EArgumentNilException = sysutils.EArgumentNilException;

  TValue = rtti.TValue;

  Nullable<T> = record
  private
    fValue: T;
    fHasValue: string;
    function GetValue: T;
    function GetHasValue: Boolean;

    /// <summary>
    /// Internal use. Marks the current instance as null.
    /// </summary>
    /// <remarks>
    /// The <see cref="Nullable&lt;T&gt;" /> type is immutable so that this
    /// method must be private.
    /// </remarks>
    procedure Clear;

    /// <summary>
    /// Determines whether a variant value is null or empty.
    /// </summary>
    class function VarIsNullOrEmpty(const value: Variant): Boolean; static;
  public
    /// <summary>
    /// Initializes a new instance of the <see cref="Nullable&lt;T&gt;" />
    /// structure to the specified value.
    /// </summary>
    constructor Create(const value: T); overload;

    /// <summary>
    /// Initializes a new instance of the <see cref="Nullable&lt;T&gt;" />
    /// structure to the specified value.
    /// </summary>
    constructor Create(const value: Variant); overload;

    /// <summary>
    /// Retrieves the value of the current <see cref="Nullable&lt;T&gt;" />
    /// object, or the object's default value.
    /// </summary>
    function GetValueOrDefault: T; overload;

    /// <summary>
    /// Retrieves the value of the current <see cref="Nullable&lt;T&gt;" />
    /// object, or the specified default value.
    /// </summary>
    /// <param name="defaultValue">
    /// A value to return if the <see cref="HasValue" /> property is
    /// <c>False</c>.
    /// </param>
    /// <returns>
    /// The value of the <see cref="Value" /> property if the
    /// <see cref="HasValue" /> property is true; otherwise, the
    /// <paramref name="defaultValue" /> parameter.
    /// </returns>
    /// <remarks>
    /// The <see cref="GetValueOrDefault" /> method returns a value even if
    /// the <see cref="HasValue" /> property is false (unlike the
    /// <see cref="Value" /> property, which throws an exception).
    /// </remarks>
    function GetValueOrDefault(const defaultValue: T): T; overload;

    /// <summary>
    /// Determines whether two nullable value are equal.
    /// </summary>
    /// <remarks>
    /// <p> If both two nullable values are null, return true; </p>
    /// <p> If either one is null, return false; </p>
    /// <p> else compares their values as usual. </p>
    /// </remarks>
    function Equals(const other: Nullable<T>): Boolean;

    /// <summary>
    /// Gets a value indicating whether the current
    /// <see cref="Nullable&lt;T&gt;" /> structure has a value.
    /// </summary>
    property HasValue: Boolean read GetHasValue;

    /// <summary>
    /// Gets the value of the current <see cref="Nullable&lt;T&gt;" /> value.
    /// </summary>
    /// <exception cref="Spring|EInvalidOperationException">
    /// Raised if the value is null.
    /// </exception>
    property value: T read GetValue;

    { Operator Overloads }
    class operator Implicit(const value: Nullable<T>): T;
    class operator Implicit(const value: T): Nullable<T>;
    class operator Implicit(const value: Nullable<T>): Variant;
    class operator Implicit(const value: Variant): Nullable<T>;
    class operator Implicit(value: Pointer): Nullable<T>;
    class operator Explicit(const value: Nullable<T>): T;
    class operator Equal(const a, b: Nullable<T>): Boolean;
    class operator NotEqual(const a, b: Nullable<T>): Boolean;
  end;

  NullableString = Nullable<string>;
  NullableInteger = Nullable<integer>;
  NullableInt64 = Nullable<Int64>;
  NullableDouble = Nullable<Double>;
  NullableSingle = Nullable<Single>;
  NullableCurrency = Nullable<Currency>;
  NullableDateTime = Nullable<TDateTime>;

implementation

const
  CHasValueFlag = '@';

constructor Nullable<T>.Create(const value: T);
begin
  fValue := value;
  fHasValue := CHasValueFlag;
end;

constructor Nullable<T>.Create(const value: Variant);
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    fValue := v.AsType<T>;
    fHasValue := CHasValueFlag;
  end
  else
    Clear;
end;

procedure Nullable<T>.Clear;
begin
  fHasValue := '';
  fValue := Default (T);
end;

class function Nullable<T>.VarIsNullOrEmpty(const value: Variant): Boolean;
begin
  Result := VarIsNull(value) or VarIsEmpty(value);
end;

function Nullable<T>.GetHasValue: Boolean;
begin
  Result := Length(fHasValue) > 0;
end;

function Nullable<T>.GetValue: T;
begin
  if not HasValue then
    raise EInvalidOperationException.CreateRes(@SNullableTypeHasNoValue);
  Result := fValue;
end;

function Nullable<T>.GetValueOrDefault: T;
begin
  if HasValue then
    Result := value
  else
    Result := Default (T);
end;

function Nullable<T>.GetValueOrDefault(const defaultValue: T): T;
begin
  if HasValue then
    Result := value
  else
    Result := defaultValue;
end;

function Nullable<T>.Equals(const other: Nullable<T>): Boolean;
begin
  if HasValue and other.HasValue then
    Result := TEqualityComparer<T>.Default.Equals(value, other.value)
  else
    Result := HasValue = other.HasValue;
end;

class operator Nullable<T>.Implicit(const value: T): Nullable<T>;
begin
  Result := Nullable<T>.Create(value);
end;

class operator Nullable<T>.Implicit(const value: Nullable<T>): T;
begin
  Result := value.value;
end;

class operator Nullable<T>.Implicit(const value: Nullable<T>): Variant;
var
  v: TValue;
begin
  if value.HasValue then
  begin
    v := TValue.From<T>(value.value);
    if v.IsType<Boolean> then
      Result := v.AsBoolean
    else
      Result := v.AsVariant;
  end
  else
    Result := Null;
end;

class operator Nullable<T>.Implicit(const value: Variant): Nullable<T>;
var
  v: TValue;
begin
  if not VarIsNullOrEmpty(value) then
  begin
    v := TValue.FromVariant(value);
    Result := Nullable<T>.Create(v.AsType<T>);
  end
  else
    Result.Clear;
end;

class operator Nullable<T>.Implicit(value: Pointer): Nullable<T>;
begin
  if not Assigned(value) then
    Result.Clear
  else
    raise EInvalidOperationException.CreateRes(@SCannotAssignPointerToNullable);
end;

class operator Nullable<T>.Explicit(const value: Nullable<T>): T;
begin
  Result := value.value;
end;

class operator Nullable<T>.Equal(const a, b: Nullable<T>): Boolean;
begin
  Result := a.Equals(b);
end;

class operator Nullable<T>.NotEqual(const a, b: Nullable<T>): Boolean;
begin
  Result := not a.Equals(b);
end;

end.
