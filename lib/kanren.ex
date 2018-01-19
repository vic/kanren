# implementing microkanren https://www.youtube.com/watch?v=0FwIwewHC3o
# microkanren with constrains https://arxiv.org/pdf/1701.00633.pdf

defmodule Kanren.List do
  def zero, do: []
  def unit(x), do: [x | zero()]

  def plus([], ys), do: ys
  def plus([x | xs], ys), do: [x | plus(xs, ys)]
  def plus(fxs, ys) when is_function(fxs), do: fn -> plus(ys, fxs.()) end

  def bind([], _f), do: zero()
  def bind([x | xs], f), do: plus(f.(x), bind(xs, f))
  def bind(l, f) when is_function(l), do: fn -> bind(l.(), f) end

  def lazy(g, _), do: fn s -> fn -> g.(s) end end
  def disj(ga, gb, _), do: fn s -> plus(ga.(s), gb.(s)) end
  def conj(g, f, _), do: fn s -> bind(g.(s), f)end
end

defmodule Kanren.Stream do
  def zero, do: Stream.concat([])
  def unit(x), do: Stream.concat([[x]])

  def plus(x, y), do: Stream.concat([x, y])
  def bind(x, f), do: Stream.map(x, f)

  def lazy(g, _) do
    fn s ->
      init = fn -> nil end
      done = fn _ -> nil end
      producer = fn _ -> {g.(s), nil} end
      Stream.resource(init, producer, done)
      |> Stream.flat_map(&(&1))
    end
  end

  def disj(ga, gb, _) do
    fn s ->
      init = fn -> :a end
      producer = fn
        :a -> {ga.(s), :b}
        :b -> {gb.(s), :c}
        :c -> {:halt, nil}
      end
      done = fn _ -> nil end
      Stream.resource(init, producer, done)
      |> Stream.flat_map(&(&1))
    end
  end

  def conj(g, f, _) do
    fn s -> g.(s) |> Stream.flat_map(f) end
  end

end

defmodule Kanren.Flow do
  def lazy(g, _) do
    fn s ->
    end
  end

  def conj(f, g, _) do
    fn s ->
      f.(s) |> Flow.from_enumerable |> Flow.flat_map(fn s -> g.(s) end)
    end
  end

  def disj(f, g, _) do
    fn s ->
      [f, g] |> Flow.from_enumerable |> Flow.flat_map(fn x -> x.(s) end)
    end
  end
end

defmodule Kanren.Var do
  defstruct v: 0
  def var(v), do: %__MODULE__{v: v}
end

defmodule Kanren.Substitution do
  alias __MODULE__, as: S
  defstruct binds: %{}, next: 0

  def empty, do: %S{}

  def fetch(%S{binds: m}, v), do: Map.get(m, v)
  def bound?(%S{binds: m}, v), do: Map.has_key?(m, v)

  def bind(s = %S{binds: m}, v, u) do
    %{s | binds: Map.put(m, v, u)}
  end

  def fresh(s = %S{next: n}, f) do
    v = f.(n)
    s = %{s | next: n + 1}
    {s, v}
  end
end

defprotocol Kanren.Relation do
  @fallback_to_any true
  def bidirectional?(x, y)
  def unifiable?(x, y, s)
  def unify(x, y, s)
  def reference?(x, s)
  def dereference(x, s)
end

defimpl Kanren.Relation, for: Any do
  def bidirectional?(_, _), do: false
  def reference?(_, _), do: false
  def dereference(_, _), do: raise("Not implemented")
  def unifiable?(_, _, _), do: false
  def unify(_, _, _), do: raise("Not implemented")
end

defimpl Kanren.Relation, for: Kanren.Var do
  alias Kanren.Substitution, as: S
  def bidirectional?(_, _), do: true
  def reference?(v, s), do: S.bound?(s, v)
  def dereference(v, s), do: S.fetch(s, v)
  def unifiable?(v, _, s), do: !reference?(v, s)
  def unify(v, u, s), do: S.bind(s, v, u)
end

defimpl Kanren.Relation, for: List do
  alias Kanren.Unify, as: U
  def bidirectional?(_, _), do: true
  def reference?(_, _), do: false
  def dereference(_, _), do: raise("Not implemented")
  def unifiable?(_, y, _), do: is_list(y)

  def unify([], [], s), do: s
  def unify([_ | _], [], _), do: nil
  def unify([], [_ | _], _), do: nil
  def unify([x | xs], [y | ys], s) do
    if s = U.unify(s, x, y) do
      U.unify(s, xs, ys)
    end
  end
end

defmodule Kanren.Unify do
  alias Kanren.List, as: L
  alias Kanren.Substitution, as: S
  alias Kanren.Relation, as: R

  def dereference(s, u) do
    if R.reference?(u, s) do
      u = R.dereference(u, s)
      dereference(s, u)
    else
      u
    end
  end

  def unify(s, u, v) do
    u = dereference(s, u)
    v = dereference(s, v)

    cond do
      u == v -> s
      R.unifiable?(u, v, s) -> R.unify(u, v, s)
      R.bidirectional?(v, u) && R.unifiable?(v, u, s) -> R.unify(v, u, s)
      :else -> nil
    end
  end

  def goal(u) do
    fn s ->
      if s = u.(s) do
        L.unit(s)
      else
        L.zero
      end
    end
  end
end

defmodule Kanren.Micro do
  alias Kanren.Unify, as: U
  alias Kanren.List, as: L
  alias Kanren.Flow, as: F

  def lazy(g, s, opts \\ [module: L]), do: opts[:module].lazy(g, s, opts)
  def conj(a, b, opts \\ [module: L]), do: opts[:module].conj(a, b, opts)
  def disj(a, b, opts \\ [module: L]), do: opts[:module].disj(a, b, opts)

  def eq(u, v), do: U.goal(&U.unify(&1, u, v))
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
  alias Kanren.Substitution, as: S
  alias Kanren.Unify, as: U
  alias Kanren.Micro, as: M
  alias Kanren.Constraint, as: C
  alias Kanren.Var, as: V

  defdelegate eq(a, b), to: M
  defdelegate disj(a, b), to: M
  defdelegate conj(a, b), to: M

  def fresh_var(f) do
    fn s ->
      {s, v} = S.fresh(s, &V.var/1)
      f.(v).(s)
    end
  end

  def goal(f), do: f

  defp locals(ast) do
    {_, vars} =
      Macro.traverse(
        ast,
        [],
        fn
          v = {x, _, y}, b when is_atom(x) and is_atom(y) ->
            lv = {{:., [], [Kanren.Var, :var]}, [], [x]}
            set = {:=, [], [v, lv]}
            {v, [set | b]}

          a, b ->
            {a, b}
        end,
        fn a, b -> {a, b} end
      )

    vars
  end

  defmacro run(opts) do
    code = Keyword.get(opts, :do)
    vars = locals(code)

    quote do
      import Kernel, only: []
      import Kanren.Operators
      import Kanren

      unquote(vars)

      K.conj(do: unquote(code)).(S.empty())
    end
  end

  defmacro conj(goals) do
    quote do: comp(M.conj(), unquote(goals))
  end

  defmacro disj(goals) do
    quote do: comp(M.disj(), unquote(goals))
  end

  defmacro fact()

  defmacro fact(do: {:__block__, [], facts}) do
    nil
  end

  defmacro fact(do: fact), do: fact

  defmacro comp(binary_goal, do: block) do
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

  defmacro lazy(g, opts \\ [module: Kanren.Stream]) do
    quote do
      fn s ->
        opts = unquote(opts)
        opts[:module].lazy(unquote(g), opts).(s)
      end
    end
  end

end
