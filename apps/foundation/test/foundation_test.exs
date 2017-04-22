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
defmodule Lkn.Foundation.Test do
  use ExUnit.Case

  doctest Lkn.Foundation.Option
  doctest Lkn.Foundation.Result

  use Lkn.Foundation

  test "inside macro" do
    x = Option.some(3)
    y = Option.none()

    Option.inside(x, v) do
      assert(v === 3)
    end

    Option.inside(y, _v) do
      assert false
    end
  end

  test "unwrap none panick" do
    try do
      Option.unwrap!(Option.none)
      assert false
    rescue
      ArgumentError -> :ok
    end
  end
end
