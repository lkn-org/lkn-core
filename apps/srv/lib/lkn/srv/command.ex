defmodule Lkn.Srv.Command do
  use Lkn.Foundation

  alias Poison.Parser

  @type m :: module

  defmodule Talk do
    defstruct [
      :msg,
    ]

    @type t :: %Talk{
      msg: String.t,
    }

    def craft!(map) do
      case Map.fetch(map, "msg") do
        {:ok, msg} ->
          %Talk{
            msg: msg,
          }
        _ ->
          raise "TALK: wrong content"
      end
    end
  end

  @spec parse!(String.t) :: Command.Talk.t
  def parse!(packet) do
    mpacket = Parser.parse!(packet)
    parser!(mpacket["type"]).craft!(mpacket["content"])
  end

  @spec parser!(String.t) :: m
  defp parser!(cmd) do
    case cmd do
      "TALK" ->
        Talk
      _ ->
        raise "#{inspect cmd}: unknown command"
    end
  end
end
