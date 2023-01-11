defmodule ChatterboxTest do
  use ExUnit.Case
  doctest Chatterbox

  test "greets the world" do
    assert Chatterbox.hello() == :world
  end
end
