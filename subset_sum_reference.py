def subset_sum(v: list[int], t: int) -> list[bool] | None:
    n = len(v)
    a = [False] * n
    i = 0
    s = 0
    advancing = True

    while i >= 0:
        # print(f"{a=} {s=}")
        if i == len(a):
            if s == t:
                return a
            else:
                i -= 1
                advancing = False
        else:
            if advancing:
                assert all(a[j] == False for j in range(i, n))
                if s + v[i] <= t:
                    a[i] = True
                    s += v[i]
                i += 1
            else:
                if a[i]:
                    a[i] = False
                    s -= v[i]
                    i += 1
                    advancing = True
                else:
                    i -= 1

    return None


v = [3, 5, 2, 6]
t = 8
a = subset_sum(v, t)
assert [vi for vi, ai in zip(v, a) if ai] == [3, 5]

# tiny warm up
v = [35598, 41872, 81980, 98583, 65116, 96540, 10035, 60706, 14417, 64505]
t = 248550
a = subset_sum(v, t)
assert [vi for vi, ai in zip(v, a) if ai] == [35598, 41872, 96540, 10035, 64505]

# multiple solutions, lex pin
v = [120, 180, 200, 150, 100, 90, 80, 70, 300, 60]
t = 300
a = subset_sum(v, t)
assert [vi for vi, ai in zip(v, a) if ai] == [120, 180]

# no solution
v = [59, 89720, 63262, 24662, 73570, 35930, 83954, 41901, 92098, 37536, 35156, 701, 33952, 7954]
t = 240322
assert subset_sum(v, t) is None

# single-element subset
v = [62554, 40915, 24211, 27558, 54959, 22322, 76841, 33232, 83608, 97109]
t = 62554
a = subset_sum(v, t)
assert [vi for vi, ai in zip(v, a) if ai] == [62554]

# last-index-required
v = [1864, 1519, 695, 1825, 290, 253, 1919, 302, 1542, 1283, 1486, 16687]
t = 16687
a = subset_sum(v, t)
assert [vi for vi, ai in zip(v, a) if ai] == [16687]

# duplicate values
v = [500, 500, 500, 300, 300, 700, 900, 500, 300, 700]
t = 1000
a = subset_sum(v, t)
assert [vi for vi, ai in zip(v, a) if ai] == [500, 500]

# near-total-sum, 20 values
v = [58443, 79693, 37155, 15450, 57084, 20590, 29841, 13454, 91581, 60485, 36863, 169, 33749, 20147, 72090, 52216, 92490, 97963, 96043, 90230]
t = 633441
a = subset_sum(v, t)
assert [vi for vi, ai in zip(v, a) if ai] == [58443, 79693, 15450, 57084, 20590, 13454, 91581, 36863, 72090, 97963, 90230]
