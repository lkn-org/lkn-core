#
# lkn.foundation: foundation of the lkn game engine
# Copyright (C) 2017 Thomas Letan <contact@thomasletan.fr>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
defmodule Lkn.Foundation.Option do
  @moduledoc """
  An `Lkn.Foundation.Option` encapsulates an optional value.

  Whenever a function may or may not returns a value, the Option
  macros should be used. In a similar manner, these macros should be
  used in pattern matching blocks. This way, the inner representation
  can change without the refactoring pain.

  Because both `Lkn.Foundation.Option.some/1` and
  `Lkn.Foundation.Option.none/0` are macros, one should `require` this
  module before using it (an alias might also be a good idea).

  ## Example

      require Lkn.Foundation.Option
      alias Lkn.Foundation.Option

      x = Option.some(3)
      y = Option.none()

      Option.unwrap(x, 1)
      #=> 3
      Option.unwrap!(x)
      #=> 3
      Option.unwrap(y, 1)
      #=> 1
  """

  @type t(x) :: {:some, x}|:none

  @doc """
  A macro to create and match an optional value.
  """
  defmacro some(x) do
    quote do
      {:some, unquote(x)}
    end
  end

  @doc """
  A macro to create and match a none value.
  """
  defmacro none do
    quote do
      :none
    end
  end

  @doc """
  A macro to execute a block only if the given optional value exists.

  In `expression`, one can use `x`: it gets the value encapsulated in
  `opt`.
  """
  defmacro inside(opt, x, do: expression) do
    quote do
      case unquote(opt) do
        unquote(some(x)) -> unquote(expression)
        unquote(none())  -> :ok
      end
    end
  end

  @spec unwrap(x:: t(X), default :: X) :: X
  @doc """
  Get the value contained within a `Lkn.Foundation.Option` or a
  default value.

      iex> Lkn.Foundation.Option.unwrap(Lkn.Foundation.Option.some(3), 1)
      3
      iex> Lkn.Foundation.Option.unwrap(Lkn.Foundation.Option.none(), 1)
      1
  """
  def unwrap(x, default) do
    case x do
      some(i) -> i
      none() -> default
    end
  end

  @spec unwrap!(x :: t(X)) :: X
  @doc """
  Same as `Lkn.Foundation.Option.unwrap/2`, but panicked if there is no
  value to unwrap.

      iex> Lkn.Foundation.Option.unwrap!(Lkn.Foundation.Option.some(3))
      3
  """
  def unwrap!(some(x)) do
    x
  end
  def unwrap!(none()) do
    raise ArgumentError, "Trying to unwrap a `none` value"
  end

  @spec map(x :: t(X), f :: (X -> Y)) :: t(Y)
  @doc """
  Apply a function to an optional value if it exists.

      iex> Lkn.Foundation.Option.map(Lkn.Foundation.Option.some(3), &(&1 + 1))
      Lkn.Foundation.Option.some(4)
  """
  def map(some(x), f) do
    some(f.(x))
  end
  def map(none(), _f) do
    none()
  end
end
