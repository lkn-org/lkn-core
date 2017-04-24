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
defmodule Lkn.Core do
  defmodule Name do
    @moduledoc false

    @type t :: term

    alias Lkn.Core.Entity
    alias Lkn.Core.Instance
    alias Lkn.Core.System

    @spec properties(Entity.t) :: t
    def properties(entity_key) do
      {:via, Registry, {Lkn.Core.Registry, {:entity, entity_key, :props}}}
    end

    @spec component(Entity.t, System.m) :: t
    def component(entity_key, system) do
      {:via, Registry, {Lkn.Core.Registry, {:entity, entity_key, system}}}
    end

    @spec comps_list(Entity.t) :: t
    def comps_list(entity_key) do
      {:via, Registry, {Lkn.Core.Registry, {:entity, entity_key, :comps_list}}}
    end

    @spec entity(Entity.t) :: t
    def entity(entity_key) do
      {:via, Registry, {Lkn.Core.Registry, {:entity, entity_key}}}
    end

    @spec system(Instance.t, System.m) :: t
    def system(instance_key, sys) do
      {:via, Registry, {Lkn.Core.Registry, {:engine, instance_key, sys}}}
    end

    @spec notify_group(Instance.t) :: t
    def notify_group(instance_key) do
      {:via, Registry, {Lkn.Core.Notifier, {:engine, instance_key, :notifier}}}
    end

    @spec instance(Instance.t) :: t
    def instance(instance_key) do
      {:via, Registry, {Lkn.Core.Registry, {:instance, instance_key}}}
    end

    @spec puppeteer(Puppeteer.k) :: t
    def puppeteer(puppeteer_key) do
      {:via, Registry, {Lkn.Core.Registry, {:puppeteer, puppeteer_key}}}
    end

    @spec pool(Entity.t) :: t
    def pool(map_key) do
      {:via, Registry, {Lkn.Core.Registry, {:pool, map_key}}}
    end

    @spec instance_sup(Entity.t) :: t
    def instance_sup(map_key) do
      {:via, Registry, {Lkn.Core.Registry, {:pool, map_key, :sup}}}
    end
  end

  @moduledoc """
  An Entity-Component-System framework delivered as an Elixir `Application`.

  ## Philosophy

  ## Example

  ### Defining A System

      defmodule LevelUpSystem do
        use Lkn.Core.System

        @callback level_up(key :: any) :: {:level_up, key :: any, old :: integer, new :: integer}

        def start_link(group) do
          Lkn.Core.System.start_link(__MODULE__, :ok, LevelUpSystem, group)
        end

        def entity_enter(:ok, _entities, key) do
          Lkn.Core.System.notify({:enter, key})
          :ok
        end

        def entity_leave(:ok, _entities, key) do
          Lkn.Core.System.notify({:leave, key})
          :ok
        end

        def system_cast({:level_up, entity_key}, entities, state) do
          notif = comp(entities, entity_key).level_up(entity_key)
          Lkn.Core.System.notify(notif)
          state
        end

        def level_up(entity_key) do
          Lkn.Core.System.cast(LevelUpSystem, {:level_up, entity_key})
        end
      end

  ### Defining An Entity

  #### Properties

  #### Defining The Component

      defmodule MyEntity.LevelUp do
        use Lkn.Core.Component, sys: LevelUpSystem

        def level_up(entity_key) do
          {:ok, lvl} = read(entity_key, :level)
          write(entity_key, :level, lvl + 1)

          {:level_up, lvl, lvl + 1}
        end
      end

  #### Defining The Actual Entity

      defmodule MyEntity do
        use Lkn.Core.Entity, components: [MyEntity.LevelUp]

        def init_properties(name: name, level: level) do
          %{:name => name, :level => level}
        end

        def start_link(key, [name: _name, level: _level] = args) do
          Lkn.Core.Entity.start_link(__MODULE__, key, args)
        end
      end
  """
  use Application

  import Supervisor.Spec

  @doc false
  def start(_type, _args) do
    Supervisor.start_link(__MODULE__, :ok)
  end

  @doc false
  @spec init(any) ::
  {:ok, {:supervisor.sup_flags, [Supervisor.Spec.spec]}} |
  :ignore
  def init(_) do
    children = [
      supervisor(Registry, [:unique, Lkn.Core.Registry], id: :core_registry),
      supervisor(Registry, [:duplicate, Lkn.Core.Notifier], id: :core_notifier),
      supervisor(Lkn.Core.Pool.Supervisor, [])
    ]

    supervise(children, strategy: :one_for_all)
  end
end
