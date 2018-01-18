defmodule KanrenTest do
  use ExUnit.Case
  require Kanren

  doctest Kanren

  alias Kanren, as: K
  alias Kanren.Substitution, as: S
  alias Kanren.Unify, as: U
  alias Kanren.Var, as: V

  describe "unify" do

    test "a fresh variable unifies to any value" do
      s =
        S.empty()
        |> U.unify(V.var(:q), 22)
        |> S.fetch(V.var(:q))
      assert s == 22
    end

    test "a non-fresh variable unifies only to its bound value" do
      s =
        S.empty()
        |> S.bind(V.var(:q), 99)
        |> U.unify(V.var(:q), 22)
      assert s == nil
    end

    test "two list bind each zipped item" do
      s =
        S.empty()
        |> U.unify([1, 2, 3], [1, V.var(:q), 3])
        |> S.fetch(V.var(:q))
      assert s == 2
    end

  end

  test "goal can return an empty list" do
    assert [] = K.run(do: goal(fn s -> [] end))
  end

  test "goal can return a non-empty list" do
    assert [_] = K.run(do: goal(fn s -> [s] end))
  end

  test "eq unifies on two equal numbers" do
    assert [_] = K.run(do: 9 == 9)
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

  @tag :skip
  test "walk on a -> b -> a"

  @tag :skip
  test "jealous fact" do
    (quote do
      fact do
        male(:vincent)
        male(q)
        loves(a, b)
        jealous(a, b) when loves(a, c) and loves(b, c)
      end
    end)
  end

end
