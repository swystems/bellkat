from __future__ import annotations
from typing import NewType, Optional, Callable
from dataclasses import dataclass
import networkx as nx
import matplotlib.pyplot as plt
import abc

Location = NewType('Location', str)


class BellPair(set[Location]):
    def __str__(self):
        match tuple(self):
            case l,:
                return f'${l},{l}$'
            case l1, l2:
                return f'${l1},{l2}$'


@dataclass
class Tree:
    pair: BellPair
    children: list[Tree]

    def to_graph(self):
        t = nx.join([(c.to_graph(), 0) for c in self.children])
        t.nodes[0]['label'] = str(self.pair)
        return t

    def to_digraph(self):
        t = self.to_graph()
        dt = nx.dfs_tree(t, 0)
        for n in t:
            dt.nodes[n]['label'] = t.nodes[n]['label']
        return dt.reverse()


class History(list[Tree]):
    def to_graph(self):
        return nx.disjoint_union_all([t.to_digraph() for t in self])

    def draw(self):
        plt.rcParams.update({'text.usetex': True})
        h = self.to_graph()
        nx.draw(h, pos=nx.nx_pydot.pydot_layout(h, prog='dot'),
                labels={n: h.nodes[n]['label'] for n in h},
                node_shape='s', node_size=1500)

    def __add__(self, other: History) -> History:
        return History(super().__add__(other))


class Policy(abc.ABC):
    @abc.abstractmethod
    def __call__(self, history: History) -> Optional[History]:
        raise NotImplementedError

    @staticmethod
    def _find_root(
            history: History,
            f: Callable[[BellPair], bool],
    ) -> Optional[tuple[Tree, History]]:
        match [t for t in history if f(t.pair)]:
            case []:
                return None
            case [t, *_]:
                rest = history.copy()
                rest.remove(t)
                return t, History(rest)

    @classmethod
    def _find_root_eq(
            cls,
            history: History,
            pair: BellPair,
    ) -> Optional[tuple[Tree, History]]:
        return cls._find_root(history, lambda p: p == pair)

    def __mul__(self, other: Policy) -> Policy:
        return SequencePolicy(self, other)

    def __add__(self, other: Policy) -> Policy:
        return UnionPolicy(self, other)


@dataclass
class UnionPolicy(Policy):
    fst: Policy
    snd: Policy

    def __call__(self, history: History) -> Optional[History]:
        match (self.fst(history), self.snd(history)):
            case None, _:
                return None
            case _, None:
                return None
            case h1, h2:
                return h1 + h2


@dataclass
class SequencePolicy(Policy):
    fst: Policy
    snd: Policy

    def __call__(self, history: History) -> Optional[History]:
        match self.fst(history):
            case None:
                return None
            case h:
                return self.snd(h)


@dataclass
class Transmit(Policy):
    src: Location
    dst: tuple[Location, Location] | Location

    def __call__(self, history: History) -> Optional[History]:
        match self._find_root_eq(history, BellPair({self.src})):
            case None:
                return None
            case (t, rest):
                match self.dst:
                    case (Location(_), Location(_)):
                        return History(
                            [Tree(BellPair(set(self.dst)), [t])]) + rest
                    case _:
                        return History(
                            [Tree(BellPair({self.dst, self.src}), [t])]) + rest


@dataclass
class Create(Policy):
    loc: Location

    def __call__(self, history: History) -> History:
        return History([Tree(BellPair({self.loc}), [])]) + history


@dataclass
class Swap(Policy):
    loc: Location
    remote: tuple[Location, Location]

    def __call__(self, history: History) -> Optional[History]:
        bps = tuple(BellPair({self.loc, r}) for r in self.remote)
        match self._find_root_eq(history, bps[0]):
            case (t1, history):
                match self._find_root_eq(history, bps[1]):
                    case (t2, history):
                        return History([Tree(BellPair(set(self.remote)),
                                             [t1, t2])]) + history


@dataclass
class Distill(Policy):
    pair: tuple[Location, Location]

    def __call__(self, history: History) -> Optional[History]:
        bp = BellPair({self.pair[0], self.pair[1]})
        match self._find_root_eq(history, bp):
            case (t1, history):
                match self._find_root_eq(history, bp):
                    case (t2, history):
                        return History([Tree(bp, [t1, t2])]) + history
