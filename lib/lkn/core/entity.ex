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

  @moduledoc false

  @spec start_link([module], Entity.k) :: Supervisor.on_start
  def start_link(comps, entity_key) do
    Supervisor.start_link(__MODULE__, {comps, entity_key})
  end

  @spec init({[System.m], Entity.k}) ::
  {:ok, {:supervisor.sup_flags, [Supervisor.Spec.spec]}} |
  :ignore
  def init({comps, entity_key}) do
    children = Enum.map(comps, fn comp_module ->
      worker(comp_module, [entity_key])
    end)

    supervise(children, strategy: :one_for_one)
  end
end

defmodule Lkn.Core.Entity do
  use Lkn.Prelude

  alias Lkn.Core.Name
  alias Lkn.Core.Properties

  @typedoc """
  A property of the Entity, e.g. its health point, its current speed, etc.
  """
  @type prop :: any

  @typedoc """
  A value associated to a given Entity's property.
  """
  @type value :: any

  @moduledoc """
  A behaviour module for implementing an Entity.

  **Note:** An Entity can either be a Map or a Puppet. To actually
    implement an Entity, you should use either
    `Lkn.Core.Puppet.defpuppet/2` or `Lkn.Core.Map.defmap/2`. In other
    words, if your code contains `@behaviour Lkn.Core.Entity`, you are
    doing it wrong. From a developer point of view, only the
    `start_link/3` function is really useful.
  """

  @typedoc """
  A key to identify and reach an Entity, that is either a
  `Lkn.Core.Puppet` or a `Lkn.Core.Map`.
  """
  @type k :: Lkn.Core.Map.k | Lkn.Core.Puppet.k
  @type digest :: term

  @typedoc """
  The third argument of the `start_link/3` function which is passed to
  the `init_properties/1` callback.
  """
  @type init_args :: any

  @doc """
  Initializes the Entity's map of properties.

  This map is used by
  """
  @callback init_properties(init_args) :: %{prop => value}
  @callback digest(entity :: %{prop => value}) :: digest
  @callback destroy(key :: k, entity :: %{prop => value}, reason :: term) :: any

  defmacro __using__(components: comps) do
    quote location: :keep do
      use Supervisor

      @behaviour Lkn.Core.Entity

      def init(key: entity_key, args: args) do
        props = init_properties(args)
        props = Map.put(props, :module, __MODULE__)

        sys = Enum.map(unquote(comps), &(&1.specs().system()))

        children = [
          worker(Agent, [fn -> sys end, [name: Name.comps_list(entity_key)]]),
          worker(Properties, [props, entity_key]),
          supervisor(Lkn.Core.Entity.Components, [unquote(comps), entity_key]),
        ]

        supervise(children, strategy: :rest_for_one)
      end
    end
  end

  @doc """
  Compute a digest which hopefully describes the entity
  """
  @spec digest(entity_key :: k) :: digest
  def digest(entity_key) do
    Option.some(mod) = Lkn.Core.Entity.read(entity_key, :module)

    Lkn.Core.Properties.compute(entity_key, &mod.digest(&1))
  end

  @doc """
  Starts an Entity process linked to the current process.
  """
  @spec start_link(module, k, init_args) :: Supervisor.on_start
  def start_link(module, key, args) do
    Supervisor.start_link(module, [key: key, args: args], name: Name.entity(key))
  end

  @doc false
  @spec has_component?(k, System.m) :: boolean
  def has_component?(key, sys) do
    Agent.get(Lkn.Core.Name.comps_list(key), &Enum.member?(&1, sys))
  end

  @doc false
  @spec systems(k) :: [System.m]
  def systems(key) do
    Agent.get(Lkn.Core.Name.comps_list(key), fn r -> r end)
  end

  @doc """
  Retreive the current value of the given property, if it exists.

  There is no `write` counterpart, because only a `Component` can
  modify it.
  """
  @spec read(k, prop) :: Option.t(value)
  def read(key, prop) do
    Lkn.Core.Properties.read(key, prop)
  end

  @doc """
  Stop the given Entity

  This
  """
  def stop(key, reason \\ :normal) do
    # clean up
    Option.some(mod) = Lkn.Core.Entity.read(key, :module)
    Lkn.Core.Properties.compute(key, &mod.destroy(key, &1, reason))

    # and exit
    Supervisor.stop(Name.entity(key), reason)
  end
end
