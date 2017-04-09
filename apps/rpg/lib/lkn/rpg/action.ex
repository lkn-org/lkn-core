defmodule Lkn.Rpg.Action do
  alias Lkn.Rpg.System

  def talk(instance_key, entity_key, msg) do
    System.Chat.talk(instance_key, entity_key, msg)
  end
end
