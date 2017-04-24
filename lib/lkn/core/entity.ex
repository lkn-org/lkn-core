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
defmodule Lkn.Core.Entity.Components do
  use Supervisor

  alias Lkn.Core.Entity
  alias Lkn.Core.System
  alias Lkn.Core.Component

  @moduledoc false

  @spec start_link([module], Entity.t) :: Supervisor.on_start
  def start_link(comps, entity_key) do
    Supervisor.start_link(__MODULE__, {comps, entity_key})
  end

  @spec init({[System.m], Entity.t}) ::
  {:ok, {:supervisor.sup_flags, [Supervisor.Spec.spec]}} |
  :ignore
  def init({comps, entity_key}) do
    children = Enum.map(comps, fn comp_module ->
      worker(Component, [comp_module, entity_key, comp_module.system()])
    end)

    supervise(children, strategy: :one_for_one)
  end
end

defmodule Lkn.Core.Entity do
  use Lkn.Prelude

  alias Lkn.Core.Name
  alias Lkn.Core.Properties

  @moduledoc """
  A behaviour module for implementing an Entity.
  """

  @type t :: any
  @type init_args :: any

  @callback init_properties(init_args) :: map

  defmacro __using__(components: comps) do
    quote location: :keep do
      use Supervisor

      @behaviour Lkn.Core.Entity

      def init(key: entity_key, args: args) do
        props = init_properties(args)

        comps_sys = Enum.reduce(unquote(comps), Map.new, &Map.put(&2, &1.system(), &1))

        children = [
          worker(Agent, [fn -> comps_sys end, [name: Name.comps_list(entity_key)]]),
          worker(Properties, [props, entity_key]),
          supervisor(Lkn.Core.Entity.Components, [unquote(comps), entity_key]),
        ]

        supervise(children, strategy: :rest_for_one)
      end
    end
  end

  @spec start_link(module, t, init_args) :: Supervisor.on_start
  def start_link(module, key, args) do
    Supervisor.start_link(module, [key: key, args: args], name: Name.entity(key))
  end

  @spec get_component(t, System.m) :: Option.t(Component.m)
  def get_component(key, sys) do
    r = Agent.get(Lkn.Core.Name.comps_list(key), &Map.fetch(&1, sys))

    case r do
      {:ok, t} -> Option.some(t)
      _ -> Option.nothing()
    end
  end

  @spec has_component?(t, System.m) :: boolean
  def has_component?(key, sys) do
    case Agent.get(Lkn.Core.Name.comps_list(key), &Map.fetch(&1, sys)) do
      {:ok, _} -> true
      _ -> false
    end
  end

  @spec systems(t) :: %{System.m => module}
  def systems(key) do
    Agent.get(Lkn.Core.Name.comps_list(key), fn r -> r end)
  end

  @spec read(t, Lkn.Core.Properties.prop) :: Option.t(Lkn.Core.Properties.value)
  def read(key, prop) do
    Lkn.Core.Properties.read(key, prop)
  end
end
