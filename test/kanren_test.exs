defmodule KanrenTest do
  use ExUnit.Case
  require Kanren

  doctest Kanren

  alias Kanren, as: K
  alias Kanren.List, as: L
  alias Kanren.Micro, as: M
  alias Kanren.Substitution, as: S
  alias Kanren.Unify, as: U
  alias Kanren.Var, as: V

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

  test "two list with non-unifiable items do not unify" do
    s = S.empty() |> U.unify([1, 2, 3], [1, 3, 2])
    assert s == nil
  end

  test "two list of differing length do not unify" do
    s = S.empty() |> U.unify([1, 2], [1, 2, 3])
    assert s == nil
  end

  test "vars are followed to its value" do
    s =
      S.empty()
      |> S.bind(V.var(:a), V.var(:b))
      |> S.bind(V.var(:b), 99)
      |> U.unify(V.var(:a), V.var(:c))
      |> S.fetch(V.var(:c))

    assert s == 99
  end

  test "var unification is bidirectonal" do
    s =
      S.empty()
      |> U.unify(V.var(:a), 22)
      |> S.fetch(V.var(:a))

    assert s == 22

    s =
      S.empty()
      |> U.unify(33, V.var(:a))
      |> S.fetch(V.var(:a))

    assert s == 33
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
    assert [%{binds: %{%V{v: :q} => 2}}] =
             (K.run do
                q == 2
              end)
  end

  test "eq is bidirectional" do
    assert [%{binds: %{%V{v: :q} => 2}}] =
             (K.run do
                2 == q
              end)
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

  test "disj unifies to every branch" do
    assert [
             %{binds: %{%V{v: :q} => 5}},
             %{binds: %{%V{v: :q} => 6}}
           ] = K.run(do: q == 5 || q == 6)
  end

  alias Kanren.Stream, as: St
  alias Kanren.Flow, as: Sf

  def fives(q) do
    St.disj(K.eq(5, q), K.lazy(fives(q), module: St), [])
  end

  test "foo" do
    q = V.var(:q)
    r = fives(q).(S.empty())
    IO.inspect(r |> Enum.take(3))
  end

  @tag :skip
  test "recursive goal unifies to lazy stream" do
    assert nil = K.run(do: fives(q)) |> Enum.take(2)
  end

  @tag :skip
  test "walk on a -> b -> a"

  @tag :skip
  test "jealous fact" do
    quote do
      fact do
        loves(:marcellus, :mia)
        loves(:vincent, :mia)
        jealous(a, b) when loves(a, c) and loves(b, c)
      end
    end
  end
end
