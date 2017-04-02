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

defmodule Lkn.Foundation.Result do
  @moduledoc """
  A `Lkn.Foundation.Result` encapsulates the result of a computation
  that may fail for some reason.
  """

  @type t(x,err) :: {:ok, x}|{:error, err}

  @doc """
  A macro to create and match a successful computation.
  """
  defmacro ok(x) do
    quote do
      {:ok, unquote(x)}
    end
  end

  @doc """
  A macro to create and match an error issued by a computation.
  """
  defmacro err(x) do
    quote do
      {:error, unquote(x)}
    end
  end

  @spec map(t(X,E), (X -> Y)) :: t(Y,E)
  @doc """
  Apply a function to the result of a successful computation or leave
  the error untouched.

      iex> Lkn.Foundation.Result.map(Lkn.Foundation.Result.ok(3), &(&1 + 1))
      Lkn.Foundation.Result.ok(4)
      iex> Lkn.Foundation.Result.map(Lkn.Foundation.Result.err(:no), &(&1 + 1))
      Lkn.Foundation.Result.err(:no)
  """
  def map(ok(x), f) do
    ok(f.(x))
  end
  def map(err(x), _f) do
    err(x)
  end
end
