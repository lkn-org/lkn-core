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
  doctest Lkn.Foundation.Recipe

  use Lkn.Foundation
  alias Lkn.Foundation.Recipe

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

  test "recipe (periodic, duration, !duration_callback, !cancel)" do
    {:ok, r} = Recipe.start_link(self())

    r |> Recipe.set_periodic_callback(3, &(send(&1, :ping)))
      |> Recipe.set_duration(10)
      |> Recipe.start

    wait_for(:ping, 4)
    wait_for(:ping, 4)
    wait_atleast(3)
  end

  test "recipe (periodic, duration, duration_callback, !cancel)" do
    {:ok, r} = Recipe.start_link(self())

    r |> Recipe.set_periodic_callback(3, &(send &1, :ping))
      |> Recipe.set_duration(10, Option.some(&(send &1, :pong)))
      |> Recipe.start

    wait_for(:ping, 4)
    wait_for(:ping, 4)
    wait_for(:pong, 3)
  end

  test "recipe (!periodic, duration, duration_callback, cancel)" do
    {:ok, r} = Recipe.start_link(self())

    r |> Recipe.set_duration(10, Option.some(&(send &1, :ping)))
      |> Recipe.set_cancel_callback(&(send &1, :pong))
      |> Recipe.start

    Recipe.cancel(r)
    wait_for(:pong, 3)
  end

  test "recipe (!periodic, duration, duration_callback, !cancel)" do
    {:ok, r} = Recipe.start_link(self())

    r |> Recipe.set_duration(5, Option.some(&(send &1, :ping)))
      |> Recipe.start

    wait_for(:ping, 6)
  end

  test "recipe (periodic, !duration, !duration_callback, !cancel)" do
    {:ok, r} = Recipe.start_link(self())

    r |> Recipe.set_periodic_callback(3, &(send &1, :ping))
      |> Recipe.start

    wait_for(:ping, 4)
    wait_for(:ping, 4)
    wait_for(:ping, 4)
    wait_for(:ping, 4)

    Recipe.cancel(r)
  end

  def wait_for(msg, before) do
    receive do
      m -> assert (m === msg)
    after before -> assert false
    end
  end

  def wait_atleast(time) do
    receive do
      Other -> assert false
    after time -> :ok
    end
  end
end
