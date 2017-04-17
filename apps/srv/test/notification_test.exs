defmodule SrvTest.Notification do
  use ExUnit.Case

  alias Lkn.Srv.Notification, as: N

  test "serialize talk" do
    N.serialize!(N.Talk.new(:id, "test"))
  end

  test "serialize wrong command" do
    N.serialize!(N.WrongCommand.new("...", "`...` is not a command"))
  end
end
