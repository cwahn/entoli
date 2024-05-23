from dataclasses import dataclass
from typing import Callable, Iterable, Dict, Tuple

from pytest import Item

from entoli.base.maybe import Just, Nothing, Maybe
from entoli.map import Map
from entoli.prelude import (
    concat,
    filter_map,
    conditioanl,
    fst,
    head,
    is_prefix_of,
    is_suffix_of,
    length,
    snd,
    init,
    tail,
)


@dataclass
class PyIdent:
    module: Iterable[str]
    mb_name: Maybe[str]

    def module_name(self) -> str:
        return ".".join(self.module)

    def full_qual_name(self) -> str:
        match self.mb_name:
            case Nothing():
                return self.module_name()
            case Just(name):
                return f"{self.module_name()}.{name}"

    def full_qual_name_parts(self) -> Iterable[str]:
        match self.mb_name:
            case Nothing():
                return self.module
            case Just(name):
                return concat([self.module, [name]])

    def is_module(self) -> bool:
        match self.mb_name:
            case Nothing():
                return True
            case Just(_):
                return False

    def is_wildcard(self) -> bool:
        # return is_suffix_of(".*", self.full_qual_name())
        return is_suffix_of(["*"], self.full_qual_name_parts())

    def includes(self, other: "PyIdent") -> bool:
        wc_striped = conditioanl(
            tail(self.full_qual_name_parts()) == ["*"],
            init(self.full_qual_name_parts()),
            self.full_qual_name_parts(),
        )

        other_parts = other.full_qual_name_parts()

        if length(wc_striped) > length(other_parts):
            return False
        else:
            return is_prefix_of(wc_striped, other_parts)

    def relative_name_to(self, other: "PyIdent") -> Maybe[str]:
        def _relative_name_to(
            self_part: Iterable[str], other_part: Iterable[str]
        ) -> Maybe[str]:
            match self_part:
                case []:
                    # raise ValueError(
                    #     f"Name of {self} is not longer than other. Cannot be referred relative to {other}."
                    # )
                    return Nothing()
                case [self_head, *self_tail]:
                    match other_part:
                        case []:
                            # return ".".join(self_part)
                            return Just(".".join(self_part))
                        case [other_head, *other_tail]:
                            if self_head != other_head:
                                # return ".".join(self_part)
                                return Just(".".join(self_part))
                            else:
                                return _relative_name_to(self_tail, other_tail)

            raise RuntimeError("Unreachable code")

        return _relative_name_to(
            self.full_qual_name_parts(), other.full_qual_name_parts()
        )

    def inverse_name_env(self, name_env: Map[str, "PyIdent"]) -> str:
        relevant_env = filter_map(
            lambda key_id: conditioanl(
                snd(key_id).includes(self), Just(key_id), Nothing()
            ),
            name_env.items(),
        )

        if length(relevant_env) == 0:
            raise ValueError(f"No relevant env for {self}, not defined or included.")
        elif length(relevant_env) > 1:
            raise ValueError(
                f"Multiple relevant env for {self}, {relevant_env}. Ambiguous."
            )
        else:
            relevant_ident = snd(head(relevant_env))
            relevent_key = fst(head(relevant_env))
            if self == relevant_ident:  # exact import or definition
                return relevent_key
            else:  # partial import
                return (
                    relevent_key + "." + self.relative_name_to(relevant_ident).unwrap()
                )


# In-file pytest tests


def test_PyIdent_construct():
    ident = PyIdent(module=["os"], mb_name=Just("os"))
    assert ident.module == ["os"]
    assert ident.mb_name == Just("os")

    ident = PyIdent(module=["os"], mb_name=Nothing())
    assert ident.module == ["os"]
    assert ident.mb_name == Nothing()


def test_PyIdent_full_qual_name():
    ident = PyIdent(module=["os"], mb_name=Just("os"))
    assert ident.full_qual_name() == "os.os"

    ident = PyIdent(module=["os"], mb_name=Nothing())
    assert ident.full_qual_name() == "os"


def test_PyIdent_full_qual_name_parts():
    ident = PyIdent(module=["os"], mb_name=Just("os"))
    assert ident.full_qual_name_parts() == ["os", "os"]

    ident = PyIdent(module=["os"], mb_name=Nothing())
    assert ident.full_qual_name_parts() == ["os"]


def test_PyIdent_is_module():
    ident = PyIdent(module=["os"], mb_name=Just("os"))
    assert not ident.is_module()

    ident = PyIdent(module=["os"], mb_name=Nothing())
    assert ident.is_module()


def test_PyIdent_is_wildcard():
    ident = PyIdent(module=["os"], mb_name=Just("os"))
    assert not ident.is_wildcard()

    ident = PyIdent(module=["os"], mb_name=Nothing())
    assert not ident.is_wildcard()

    ident = PyIdent(module=["os"], mb_name=Just("*"))
    assert ident.is_wildcard()

    # Not considering qualified names.
    # This is for module-level or upper.
    # ident = PyIdent(module=["os"], mb_name=Just("os.*"))
    # assert ident.is_wildcard()


def test_PyIdent_includes():
    ident = PyIdent(module=["os"], mb_name=Just("os"))
    other = PyIdent(module=["os"], mb_name=Just("os"))
    assert ident.includes(other)

    # Not considering qualified names.
    # This is for module-level or upper.

    ident = PyIdent(module=["os"], mb_name=Just("os"))
    other = PyIdent(module=["os"], mb_name=Nothing())
    assert not ident.includes(other)

    ident = PyIdent(module=["os"], mb_name=Nothing())
    other = PyIdent(module=["os"], mb_name=Just("os"))
    assert ident.includes(other)

    # ident = PyIdent(module=["os"], mb_name=Just("os"))
    # other = PyIdent(module=["os"], mb_name=Just("os.path"))
    # assert ident.includes(other)

    # ident = PyIdent(module=["os"], mb_name=Just("os.path"))
    # other = PyIdent(module=["os"], mb_name=Just("os"))
    # assert not ident.includes(other)

    # ident = PyIdent(module=["os"], mb_name=Just("os.path"))
    # other = PyIdent(module=["os"], mb_name=Just("os.path"))
    # assert ident.includes(other)

    # ident = PyIdent(module=["os"], mb_name=Just("os.path"))
    # other = PyIdent(module=["os"], mb_name=Just("os.path.join"))
    # assert ident.includes(other)

    # ident = PyIdent(module=["os"], mb_name=Just("os.path.join"))
    # other = PyIdent(module=["os"], mb_name=Just("os.path"))
    # assert not ident.includes(other)


def test_PyIdent_relative_name_to():
    # Qualified names are not considered.
    # This is for module-level or upper.

    ident = PyIdent(module=["os"], mb_name=Just("os"))
    other = PyIdent(module=["os"], mb_name=Nothing())
    assert ident.relative_name_to(other) == Just("os")

    ident = PyIdent(module=["os"], mb_name=Nothing())
    other = PyIdent(module=["os"], mb_name=Just("os"))
    assert ident.relative_name_to(other) == Nothing()

    ident = PyIdent(module=["os"], mb_name=Nothing())
    other = PyIdent(module=["os", "path"], mb_name=Just("path"))
    assert ident.relative_name_to(other) == Nothing()

    ident = PyIdent(module=["os", "path"], mb_name=Just("path"))
    other = PyIdent(module=["os"], mb_name=Nothing())
    assert ident.relative_name_to(other) == Just("path.path")


def test_PyIdent_inverse_name_env():
    name_env = Map(
        [
            ("os", PyIdent(module=["os"], mb_name=Just("os"))),
            ("os.path", PyIdent(module=["os"], mb_name=Just("os.path"))),
            ("os.path.join", PyIdent(module=["os"], mb_name=Just("os.path.join"))),
        ]
    )

    ident = PyIdent(module=["os"], mb_name=Just("os"))
    assert ident.inverse_name_env(name_env) == "os"

    ident = PyIdent(module=["os"], mb_name=Just("os.path"))
    assert ident.inverse_name_env(name_env) == "os.path"

    ident = PyIdent(module=["os"], mb_name=Just("os.path.join"))
    assert ident.inverse_name_env(name_env) == "os.path.join"
