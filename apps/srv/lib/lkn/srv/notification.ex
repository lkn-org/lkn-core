defmodule Lkn.Srv.Notification do
  defmacro __using__(defstruct: list, opcode: op) do
    quote do
      @derive [Poison.Encoder]
      defstruct unquote(list)
      def serialize!(notif = %__MODULE__{}) do
        Poison.encode!(
          %{
            type: unquote(op),
            content: notif
          }
        )
      end
    end
  end

  def serialize!(cmd = %x{}) do
    x.serialize!(cmd)
  end
end


defmodule Lkn.Srv.Notification.Talk do
  alias Lkn.Srv.Notification.Talk

  use Lkn.Srv.Notification,
    defstruct: [
      :puppet_key,
      :msg,
    ],
    opcode: "TALK"

  @type t :: %Talk{
    puppet_key: Entity.t,
    msg: String.t,
  }

  @spec new(Entity.t, String.t) :: t
  def new(pk, msg) do
    %Talk{
      puppet_key: pk,
      msg: msg,
    }
  end
end

defmodule Lkn.Srv.Notification.WrongCommand do
  alias Lkn.Srv.Notification.WrongCommand

  use Lkn.Srv.Notification,
    defstruct: [
      :cmd,
      :msg,
    ],
    opcode: "WRONG_COMMAND"

  @type t :: %WrongCommand{
    cmd: String.t,
    msg: String.t,
  }

  @spec new(String.t, String.t) :: t
  def new(cmd, msg) do
    %WrongCommand{
      cmd: cmd,
      msg: msg,
    }
  end
end
