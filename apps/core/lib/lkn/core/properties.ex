#
# lkn.core: an entity-component-system (ecs) implementation for lyxan
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
defmodule Lkn.Core.Properties do
  use Lkn.Foundation

  @moduledoc """
  Underlying properties of an `Lkn.Entity`.
  """

  @type prop :: any
  @type value :: any

  @spec start_link(map, Lkn.Core.Entity.t) :: Agent.on_start
  @doc """
  Start a new Agent for the Entity identified as `key`.
  """
  def start_link(content, entity_key) do
    Agent.start_link(fn -> content end, name: Lkn.Core.Name.properties(entity_key))
  end

  @spec read(Lkn.Core.Entity.t, prop) :: Option.t(value)
  @doc """
  Read the value of the given `prop`.
  """
  def read(entity_key, prop) do
    case Agent.get(Lkn.Core.Name.properties(entity_key), &Map.fetch(&1, prop)) do
      {:ok, val} -> Option.some(val)
      :error -> Option.none()
    end
  end

  @spec write(Lkn.Core.Entity.t, prop, value) :: :ok
  @doc """
  Update the value of the given `prop`.
  """
  def write(entity_key, prop, v) do
    Agent.update(Lkn.Core.Name.properties(entity_key), &Map.put(&1, prop, v))
  end
end
