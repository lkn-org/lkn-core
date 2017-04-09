defmodule Lkn.Rpg.Puppet do
  use Lkn.Foundation

  alias Lkn.Core.Component
  alias Lkn.Core.Entity
  alias Lkn.Rpg.System

  require Lkn.Core.Component

  defmodule Chat do
    Component.puppetify for: System.Chat

    def mute?(key) do
      Option.unwrap! read(key, :mute)
    end

    def mute(key) do
      write(key, :mute, true)
    end

    def unmute(key) do
      write(key, :mute, false)
    end
  end

  use Entity, components: [Chat]

  def init_properties(mute: mute) do
    %{
      :mute => mute,
    }
  end

  @spec start_link(Entity.t, [mute: boolean]) :: Supervisor.on_start
  def start_link(map_key, args) do
    Entity.start_link(__MODULE__, map_key, args)
  end
end
