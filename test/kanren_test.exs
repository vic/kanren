defmodule KanrenTest do
  use ExUnit.Case
  doctest Kanren

  test "greets the world" do
    assert Kanren.hello() == :world
  end
end
