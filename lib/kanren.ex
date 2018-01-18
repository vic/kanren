# microkanren with constrains https://arxiv.org/pdf/1701.00633.pdf
# arrows http://www.euclideanspace.com/maths/discrete/category/principles/arrow/index.htm

defmodule Kanren.Flow do
  def zero, do: []
  def unit(x), do: [x]

  def conj(f, g) do
    fn s ->
      f.(s) |> Flow.from_enumerable |> Flow.flat_map(fn s -> g.(s) end)
    end
  end

  def disj(f, g) do
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
  @moduledoc """
  A Substitution is Kanren's state.

  A substitution acts as a store for relationships
  between values on the system.
  """

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
  def unifiable?(x, y)
  def unify(x, y, s)
  def walkable?(x, s)
  def walk(x, s)
end

defimpl Kanren.Relation, for: Any do
  def bidirectional?(_, _), do: false
  def walkable?(_, _), do: false
  def walk(_, _), do: raise("Not implemented")
  def unifiable?(_, _), do: false
  def unify(_, _, _), do: raise("Not implemented")
end

defimpl Kanren.Relation, for: Kanren.Var do
  alias Kanren.Substitution, as: S
  def bidirectional?(_, _), do: true
  def walkable?(v, s), do: S.bound?(s, v)
  def walk(v, s), do: S.fetch(s, v)
  def unifiable?(v, s), do: true
  def unify(v, u, s), do: S.bind(s, v, u)
end

defimpl Kanren.Relation, for: List do
  alias Kanren.Unify, as: U
  def bidirectional?(_, _), do: true
  def walkable?(_, _), do: false
  def walk(_, _), do: raise("Not implemented")
  def unifiable?(x, y), do: is_list(y) and length(x) == length(y)

  def unify([x | xs], [y | ys], s) do
    if s = U.unify(s, x, y) do
      U.unify(s, xs, ys)
    end
  end
end

defmodule Kanren.Unify do
  alias Kanren.Flow, as: F
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
      R.unifiable?(u, v) -> R.unify(u, v, s)
      R.bidirectional?(v, u) && R.unifiable?(v, u) -> R.unify(v, u, s)
      :else -> nil
    end
  end

  def solver(f) do
    fn s ->
      if s = f.(s) do
        F.unit(s)
      else
        F.zero
      end
    end
  end
end

defmodule Kanren.Micro do
  alias Kanren.Unify, as: U

  @join_opts [join: Kanren.Flow]
  def conj(a, b, opts \\ @join_opts), do: opts[:join].conj(a, b)
  def disj(a, b, opts \\ @join_opts), do: opts[:join].disj(a, b)

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

    take = case Keyword.get(opts, :take, :all) do
             :all -> quote(do: Enum.to_list)
             x when is_integer(x) -> quote(do: Enum.take(unquote(x)))
             _ -> quote(do: (fn x -> x end).())
           end

    quote do
      import Kernel, only: []
      import Kanren.Operators
      import Kanren

      unquote(vars)

      K.conj(do: unquote(code)).(S.empty()) |> unquote(take)
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

  defmacro lazy(g) do
    quote do
      fn s ->
        # fn {:cont, xs}, _ ->
          # IO.inspect(s)
          # Flow.from_enumerables(xs, unquote(g).(s))
          # {:halt, Flow.from_enumerables([xs, unquote(g).(s)])}
        # {:cont, Flow.from_enumerable(xs) }
        #end
      end
    end
  end

  def fives(q) do
    disj(eq(5, q), lazy(fives(q)))
  end

end
