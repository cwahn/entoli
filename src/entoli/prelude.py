from typing import Tuple, TypeVar, Iterable, Callable, Optional, Iterator
import functools

from entoli.base.seq import Seq

_A = TypeVar("_A")
_B = TypeVar("_B")

# map and filter are already built-in functions in Python


def for_each(f: Callable[[_A], None], xs: Iterable[_A]) -> None:
    for x in xs:
        f(x)


def map(f: Callable[[_A], _B], xs: Iterable[_A]) -> Iterable[_B]:
    return Seq(lambda: (f(x) for x in xs))


def filter(f: Callable[[_A], bool], xs: Iterable[_A]) -> Iterable[_A]:
    return Seq(lambda: (x for x in xs if f(x)))


# def filter_map(f: Callable[[_A], Optional[_A]], xs: Iterable[_A]) -> Iterator[_A]:
#     for x in xs:
#         if (y := f(x)) is not None:
#             yield y


def filter_map(f: Callable[[_A], Optional[_B]], xs: Iterable[_A]) -> Iterable[_B]:
    return Seq(lambda: (y for x in xs if (y := f(x)) is not None))


def foldl(f: Callable[[_A, _B], _A], acc: _A, xs: Iterable[_B]) -> _A:
    return functools.reduce(f, xs, acc)


def head(xs: Iterable[_A]) -> _A:
    return next(iter(xs))


# def tail(xs: Iterable[_A]) -> Iterator[_A]:
#     it = iter(xs)
#     next(it)
#     return it


def tail(xs: Iterable[_A]) -> Iterable[_A]:
    def tail_():
        it = iter(xs)
        next(it)
        return it

    return Seq(tail_)


# def init(xs: Iterable[_A]) -> Iterator[_A]:
#     it = iter(xs)
#     prev = next(it)
#     for curr in it:
#         yield prev
#         prev = curr


def init(xs: Iterable[_A]) -> Iterable[_A]:
    def init_():
        it = iter(xs)
        prev = next(it)
        for curr in it:
            yield prev
            prev = curr

    return Seq(init_)


def last(xs: Iterable[_A]) -> _A:
    return functools.reduce(lambda _, x: x, xs)


def length(xs: Iterable[_A]) -> int:
    return sum(1 for _ in xs)


def null(xs: Iterable[_A]) -> bool:
    return not any(True for _ in xs)


# def reverse(xs: Iterable[_A]) -> Iterator[_A]:
#     return iter(reversed(list(xs)))


def reverse(xs: Iterable[_A]) -> Iterable[_A]:
    return Seq(lambda: reversed(list(xs)))


# def take(n: int, xs: Iterable[_A]) -> Iterator[_A]:
#     it = iter(xs)
#     for _ in range(n):
#         yield next(it)


def take(n: int, xs: Iterable[_A]) -> Iterable[_A]:
    def take_():
        it = iter(xs)
        for _ in range(n):
            yield next(it)

    return Seq(take_)


# def drop(n: int, xs: Iterable[_A]) -> Iterator[_A]:
#     it = iter(xs)
#     for _ in range(n):
#         next(it)
#     return it


def drop(n: int, xs: Iterable[_A]) -> Iterable[_A]:
    def drop_():
        it = iter(xs)
        for _ in range(n):
            next(it)
        return it

    return Seq(drop_)


def elem(x: _A, xs: Iterable[_A]) -> bool:
    return x in xs


def not_elem(x: _A, xs: Iterable[_A]) -> bool:
    return x not in xs


# def unzip(pairs: Iterable[Tuple[_A, _B]]) -> Tuple[Iterator[_A], Iterator[_B]]:
#     a, b = zip(*pairs)
#     return iter(a), iter(b)  # type: ignore


def unzip(pairs: Iterable[Tuple[_A, _B]]) -> Tuple[Iterable[_A], Iterable[_B]]:
    a, b = zip(*pairs)
    return Seq(lambda: iter(a)), Seq(lambda: iter(b))


# def concat(xss: Iterable[Iterable[_A]]) -> Iterator[_A]:
#     for xs in xss:
#         for x in xs:
#             yield x


def concat(xss: Iterable[Iterable[_A]]) -> Iterable[_A]:
    return Seq(lambda: (x for xs in xss for x in xs))


# def intersperse(x: _A, xs: Iterable[_A]) -> Iterator[_A]:
#     it = iter(xs)
#     try:
#         yield next(it)
#         for y in it:
#             yield x
#             yield y
#     except StopIteration:
#         return


def intersperse(x: _A, xs: Iterable[_A]) -> Iterable[_A]:
    def intersperse_():
        it = iter(xs)
        try:
            yield next(it)
            for y in it:
                yield x
                yield y
        except StopIteration:
            return

    return Seq(intersperse_)


# def intercalate(sep: Iterable[_A], xss: Iterable[Iterable[_A]]) -> Iterator[_A]:
#     it = iter(xss)
#     try:
#         yield from next(it)
#         for xs in it:
#             yield from sep
#             yield from xs
#     except StopIteration:
#         return


def intercalate(sep: Iterable[_A], xss: Iterable[Iterable[_A]]) -> Iterable[_A]:
    def intercalate_():
        it = iter(xss)
        try:
            yield from next(it)
            for xs in it:
                yield from sep
                yield from xs
        except StopIteration:
            return

    return Seq(intercalate_)


# def transpose(xss: Iterable[Iterable[_A]]) -> Iterator[Iterator[_A]]:
#     if not xss:
#         return iter([])
#     return iter(map(iter, zip(*xss)))


def transpose(xss: Iterable[Iterable[_A]]) -> Iterable[Iterable[_A]]:
    if not xss:
        return Seq(lambda: iter([]))
    return Seq(lambda: (iter(x) for x in zip(*xss)))


# Additional functions


# def unique(seq: Iterable[_A]) -> Iterator[_A]:
#     seen = set()
#     for x in seq:
#         if x not in seen:
#             seen.add(x)
#             yield x


def unique(seq: Iterable[_A]) -> Iterable[_A]:
    return Seq(lambda: (x for x in set(seq)))


# def sort(seq: Iterable[_A]) -> Iterator[_A]:
#     return iter(sorted(seq))  # type: ignore


def sort(seq: Iterable[_A]) -> Iterable[_A]:
    return Seq(lambda: sorted(seq))  # type: ignore


# def sort_on(f: Callable[[_A], _B], seq: Iterable[_A]) -> Iterator[_A]:
#     return iter(sorted(seq, key=f))  # type: ignore


def sort_on(f: Callable[[_A], _B], seq: Iterable[_A]) -> Iterable[_A]:
    return Seq(lambda: sorted(seq, key=f))  # type: ignore