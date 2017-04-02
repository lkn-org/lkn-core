#
# lkn.foundation: foundation of the lkn game engine
# Copyright (C) 2017 Thomas “lthms” Letan <contact@thomasletan.fr>
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
defmodule Lkn.Foundation.Recipe do
  use Lkn.Foundation
  use GenServer

  @moduledoc """
  An autonomous task with several callbacks.

  ## Examples

  When configuring the `Recipe`, the `|>` operator can be used to
  write a cleaner code.

      {:ok, r} = Recipe.start_link(self())

      r |> Recipe.set_periodic_callback(3, &(send(&1, :ping)))
        |> Recipe.set_duration(10)
        |> Recipe.start
  """

  @type t :: pid
  @type target :: pid
  @type callback :: (target -> no_return)

  defmodule State do
    alias Lkn.Foundation.Recipe
    @moduledoc false

    defstruct [
      :started,
      :target,
      :duration,
      :period,
      :periodic_callback,
      :term_callback,
      :cancel_callback,
    ]

    @type t :: %State{
      started: boolean,
      target: Recipe.target | (),
      duration: integer | (),
      period: integer | (),
      periodic_callback: Recipe.callback | (),
      term_callback: Recipe.callback | (),
      cancel_callback: Recipe.callback | (),
    }

    @spec new(Recipe.target) :: t
    def new(target) do
      %State{started: false, target: target}
    end

    @spec set_periodic(t, integer, Recipe.callback) :: t
    def set_periodic(state = %State{started: false}, period, callback) do
      %State{state|period: period, periodic_callback: callback}
    end
    def set_periodic(state = %State{started: true}, _period, _callback) do
      state
    end

    @spec set_duration(t, integer) :: t
    def set_duration(state = %State{started: false}, duration) do
      %State{state|duration: duration}
    end
    def set_duration(state = %State{started: true}, _duration) do
      state
    end

    @spec set_term(t, Recipe.callback) :: t
    def set_term(state = %State{started: false}, callback) do
      %State{state|term_callback: callback}
    end
    def set_term(state = %State{started: true}, _callback) do
      state
    end
  end

  @spec start_link(target, GenServer.options) :: GenServer.on_start
  @doc """
  Start a new Recipe that will reach the given `target` in the future.

  A Recipe cannot be used right after its creation but needs to be
  configured first. For that, the Recipe setters (such
  as`set_periodic_callback/3`, `set_duration/3`, etc.) have to be used
  before `start/1` can be called.
  """
  def start_link(target, opt \\ []) do
    GenServer.start_link(__MODULE__, State.new(target), opt)
  end

  @spec set_periodic_callback(t, integer, callback) :: t
  @doc """
  Set a periodic callback to a given Recipe.

  Once started, the Recipe will periodically executes the `callback`
  until its termination.

  If the Recipte has already been started (using `start/1`), then this
  call is effectless. If `set_periodic_callback/3` has already been
  called but the Recipe has not been started, then the new call
  overwrite the Recipe state.

  The several setters provided by this module can be chained using the
  pipe operator.
  """
  def set_periodic_callback(recipe, period, callback) do
    GenServer.cast(recipe, {:set_periodic_callback, period, callback})
    recipe
  end

  @spec set_duration(t, integer, Option.t(callback)) :: t
  @doc """
  Set a duration to a given Recipe and optionally a callback.

  Once started, the Recipe will end after `duration` milliseconds. If
  a callback has been defined, then the it is executed.

  If the Recipte has already been started (using `start/1`), then this
  call is effectless. If `set_duration/3` has already been
  called but the Recipe has not been started, then the new call
  overwrite the Recipe state.

  The several setters provided by this module can be chained using the
  pipe operator.
  """
  def set_duration(recipe, duration, callback \\ Option.none()) do
    GenServer.cast(recipe, {:set_duration, duration})

    Option.inside(callback, c) do
      GenServer.cast(recipe, {:set_term_callback, c})
    end

    recipe
  end

  @spec set_cancel_callback(t, callback) :: :ok
  @doc """
  Set a callback to execute in case the Recipe is cancelled using
  `cancel/1`.

  If the Recipte has already been started (using `start/1`), then this
  call is effectless. If `set_cancel_callback/3` has already been
  called but the Recipe has not been started, then the new call
  overwrite the Recipe state.

  The several setters provided by this module can be chained using the
  pipe operator.
  """
  def set_cancel_callback(recipe, callback) do
    GenServer.cast(recipe, {:set_cancel_callback, callback})
  end

  @spec start(t) :: :ok
  @doc """
  Start the execution of the given `Recipe`.
  """
  def start(recipe) do
    GenServer.cast(recipe, :start)
  end

  @spec cancel(t) :: :ok
  @doc """
  Cancel a Recipe before it has a chance to end.

  The regular workflow for `cancel/1` is to be called *after*
  `start/1`. Yet, for some reason, one might want to "cancel" a
  `Recipe` *before* (that is, terminates the underlying `GenServer`).
  """
  def cancel(recipe) do
    GenServer.cast(recipe, :cancel)
  end

  def handle_cast(:start, state) do
    case state do
      %State{period: nil, duration: d} ->
        Process.send_after(self(), :fire_term, d)
      %State{period: p, duration: nil} ->
        Process.send_after(self(), :fire_periodic, p)
      %State{period: p, duration: d} ->
        Process.send_after(self(), :fire_periodic, p)
        Process.send_after(self(), :fire_term, d)
    end
    {:noreply, state}
  end

  def handle_cast({:set_periodic_callback, period, callback}, state) do
    {:noreply, State.set_periodic(state, period, callback)}
  end

  def handle_cast({:set_cancel_callback, c}, state) do
    {:noreply, %{state | cancel_callback: c}}
  end

  def handle_cast({:set_duration, duration}, state) do
    {:noreply, State.set_duration(state, duration)}
  end

  def handle_cast({:set_term_callback, c}, state) do
    {:noreply, State.set_term(state, c)}
  end

  def handle_cast(:cancel, state) do
    case state do
      %State{cancel_callback: nil} ->
        :ok
      %State{cancel_callback: eff, target: t} ->
        eff.(t)
    end

    {:stop, :normal, state}
  end

  def handle_info(:fire_periodic, state) do
    case state do
      %State{period: p, periodic_callback: c, target: t} ->
        c.(t)
        Process.send_after(self(), :fire_periodic, p)
    end
    {:noreply, state}
  end

  def handle_info(:fire_term, state) do
    case state do
      %State{term_callback: nil} ->
        :ok
      %State{term_callback: c, target: t} ->
        c.(t)
    end

    {:stop, :normal, state}
  end
end
