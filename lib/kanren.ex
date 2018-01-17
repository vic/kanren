defmodule Kanren.ProperList do
  @moduledoc """
  A monadic proper-list for kanren states.

  This m-list's `plus` and `bind` operations
  support head-thunks as a way to
  trampoline computation between branches.
  """

  def zero, do: []
  def unit(x), do: [x | zero()]

  def plus([], ys), do: ys
  def plus([x | xs], ys), do: [x | plus(xs, ys)]
  def plus(f, ys) when is_function(f), do: fn -> plus(ys, f.()) end

  def bind([], _g), do: zero()
  def bind([x | xs], g), do: plus(g.(x), bind(xs, g))
  def bind(f, g) when is_function(f), do: fn -> bind(f.(), g) end
end

defmodule Kanren.Var do
  defstruct v: 0
  def var(v), do: %__MODULE__{v: v}
end

defmodule Kanren.Substitution do
  @moduledoc """
  An in-memory logical variable store.
  """

  alias Kanren.Var, as: V
  alias __MODULE__, as: S
  defstruct binds: %{}, next_var: 0

  def empty, do: %S{}
  def binding(b), do: binding(empty(), Enum.into(b, []))

  defp binding(s, []), do: s

  defp binding(s = %{binds: m}, [{k, u} | rest]) do
    binding(bind(s, V.var(k), u), rest)
  end

  def fetch(%S{binds: m}, v = %V{}), do: Map.get(m, v)
  def bound?(%S{binds: m}, v = %V{}), do: Map.has_key?(m, v)

  def bind(s = %S{binds: m}, v = %V{}, u) do
    %{s | binds: Map.put(m, v, u)}
  end

  def fresh(s = %S{next_var: n}) do
    v = V.var(n)
    s = %{s | next_var: n + 1}
    {s, v}
  end
end

defprotocol Kanren.Relation do
  @fallback_to_any true

  def traversable?(x, y)
  def traverse(x, y, s)

  def walkable?(x, s)
  def walk(x, s)
end

defimpl Kanren.Relation, for: Any do
  def walkable?(_, _), do: false
  def walk(_, _), do: raise("Not implemented")
  def traversable?(_, _), do: false
  def traverse(_, _, _), do: raise("Not implemented")
end

defimpl Kanren.Relation, for: Kanren.Var do
  alias Kanren.Substitution, as: S

  def walkable?(v, s), do: S.bound?(s, v)
  def walk(v, s), do: S.fetch(s, v)
  def traversable?(v, s), do: true
  def traverse(v, u, s), do: S.bind(s, v, u)
end

defimpl Kanren.Relation, for: List do
  alias Kanren.Unify, as: U
  def walkable?(_, _), do: false
  def walk(_, _), do: raise("Not implemented")
  def traversable?(x, y), do: is_list(x) and is_list(y)

  def traverse([x | xs], [y | ys], s) do
    if s = U.unify(s, x, y) do
      U.unify(s, xs, ys)
    end
  end
end

defmodule Kanren.Constraint do
  defstruct [:name, :predicate]
end

defmodule Kanren.Unify do
  alias Kanren.ProperList, as: L
  alias Kanren.Substitution, as: S
  alias Kanren.Relation, as: R

  def walk(s, u) do
    if R.walkable?(u, s) do
      u = R.walk(u, s)
      walk(s, u)
    else
      u
    end
  end

  def unify(s, u, v) do
    u = walk(s, u)
    v = walk(s, v)

    cond do
      u == v -> s
      R.traversable?(u, v) -> R.traverse(u, v, s)
      R.traversable?(v, u) -> R.traverse(v, u, s)
      :else -> nil
    end
  end

  def solver(f) do
    fn s ->
      if s = f.(s) do
        L.unit(s)
      else
        L.zero()
      end
    end
  end
end

defmodule Kanren.Micro do
  alias Kanren.ProperList, as: L
  alias Kanren.Unify, as: U

  def disj(a, b), do: fn s -> L.plus(a.(s), b.(s)) end
  def conj(a, b), do: fn s -> L.bind(a.(s), b) end
  def eq(u, v), do: U.solver(fn s -> U.unify(s, u, v) end)
end

defmodule Kanren.Operators do
  import Kernel, only: [def: 2]
  alias Kanren.Micro, as: M

  def a == b do
    M.eq(a, b)
  end

  def a || b do
    M.disj(a, b)
  end

  def a && b do
    M.conj(a, b)
  end
end

defmodule Kanren.Mini do
  # disj+
  # conj+
  # conde
  # fresh
  # run
  # membero
end

defmodule Kanren do
  @moduledoc """
  Documentation for Kanren.
  """

  alias __MODULE__, as: K
  alias Kanren.ProperList, as: L
  alias Kanren.Substitution, as: S
  alias Kanren.Unify, as: U
  alias Kanren.Micro, as: M
  alias Kanren.Constraint, as: C
  alias Kanren.Var, as: V

  defdelegate eq(a, b), to: M
  defdelegate disj(a, b), to: M
  defdelegate conj(a, b), to: M

  def fresh_goal(f) do
    fn s ->
      {s, v} = S.fresh(s)
      f.(v).(s)
    end
  end

  def goal(f), do: f

  defmacro run(opts) do
    {_, vars} = Macro.traverse(opts, [], fn
      v = {x, _, y}, b when is_atom(x) and is_atom(y) ->
        lv = {{:., [], [Kanren.Var, :var]}, [], [x]}
        set = {:=, [], [v, lv]}
        {v, [set | b]}
      a, b -> {a, b}
    end, fn a, b -> {a, b} end)

    quote do
      import Kernel, only: []
      import Kanren.Operators
      import Kanren
      unquote(vars)
      K.conj(unquote(opts)).(S.empty())
    end
  end

  defmacro conj(goals) do
    quote do: many(M.conj(), unquote(goals))
  end

  defmacro disj(goals) do
    quote do: many(M.disj(), unquote(goals))
  end

  defmacro many(binary_goal, do: block) do
    block
    |> case do
      {:__block__, _, block} -> block
      expr -> [expr]
    end
    |> Enum.reverse()
    |> Enum.reduce(fn a, b ->
      x = Macro.pipe(a, binary_goal, 0)
      Macro.pipe(b, x, 1)
    end)
  end
end
