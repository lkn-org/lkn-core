defmodule Lkn.Rpg.Map do
  use Lkn.Foundation

  alias Lkn.Core.Component
  alias Lkn.Core.Entity
  alias Lkn.Rpg.System

  require Lkn.Core.Component

  defmodule Chat do
    Component.mapify for: System.Chat

    def mute?(map_key) do
      Option.unwrap! read(map_key, :mute)
    end
  end

  use Entity, components: [Chat]

  def init_properties(delay: delay, limit: n, mute: mute) do
    %{
      :delay => delay,
      :limit => n,
      :mute => mute
    }
  end

  @spec start_link(Entity.t, [delay: number, limit: number, mute: boolean]) :: Supervisor.on_start
  def start_link(map_key, args) do
    Entity.start_link(__MODULE__, map_key, args)
  end
end
