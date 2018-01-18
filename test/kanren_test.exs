defmodule KanrenTest do
  use ExUnit.Case
  require Kanren

  doctest Kanren

  alias Kanren, as: K
  alias Kanren.Substitution, as: S
  alias Kanren.Var, as: V

  test "goal can return an empty list" do
    assert [] = K.run(do: goal(fn s -> [] end))
  end

  test "goal can return a non-empty list" do
    assert [%S{binds: %{}}] = K.run(do: goal(fn s -> [s] end))
  end

  test "eq unifies on two equal numbers" do
    assert [%S{binds: %{}}] = K.run(do: 9 == 9)
  end

  test "eq does not unify on two different numbers" do
    assert [] = K.run(do: 9 == 8)
  end

  test "eq can bind a variable" do
    assert [%{binds: %{%V{v: :q} => 2}}] = (K.run do q == 2 end)
  end

  test "eq is bidirectional" do
    assert [%{binds: %{%V{v: :q} => 2}}] = (K.run do 2 == q end)
  end

  test "conj unifies second if first also unifies" do
    assert [_] = K.run(do: conj(1 == 1, 2 == 2))
  end

  test "conj does not unify second if first doesnt unifies" do
    assert [] = K.run(do: conj(1 == 2, 2 == 2))
  end

  test "disj unifies second even if first does not" do
    assert [_] = K.run(do: disj(1 == 2, 2 == 2))
  end

end
