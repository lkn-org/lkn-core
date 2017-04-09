defmodule Lkn.Rpg.System.Chat do
  alias Lkn.Core.Entity
  alias Lkn.Core.System

  defmodule Puppet do
    @callback mute?(p :: Entity.t) :: boolean
    @callback mute(p :: Entity.t) :: :ok
    @callback unmute(p :: Entity.t) :: :ok
  end

  defmodule Map do
    @callback mute?(m :: Entity.t) :: :ok
  end

  defmodule State do
    @moduledoc false

    defstruct [
      :mute,
    ]

    @type t :: %State {
      mute: boolean
    }

    @spec new(boolean) :: t
    def new(mute) do
      %State{
        mute: mute
      }
    end
  end

  use System,
    state: %State{},
    puppet_component: Puppet,
    map_component: Map

  def init_state(map_key, map_comp) do
    mute = map_comp.mute?(map_key)

    State.new(mute)
  end

  def talk(instance_key, puppet_key, msg) do
    cast(instance_key, {:talk, puppet_key, msg})
  end

  def system_cast(cmd = {:talk, puppet_key, _msg}, entities, state) do
    # can we talk on this instance?
    if !state.mute do
      # can this puppet talk?
      if !comp(entities, puppet_key).mute?(puppet_key) do
        System.notify(cmd)
      end
    end
  end
end
