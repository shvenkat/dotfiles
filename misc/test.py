def foo(x: float, t: int) -> bool:
    """Checks whether some value meets a given threshold."""
    return x <= t >= 1


foo(1.2, 2)  # False.
foo(3.2, 2)  # True.

if __name__ == "__main__":
    foo(1., 1)
