# Downhill Simplex Optimization

This repository contains a basic implementation of the Downhill Simplex optimizer.
The tests contain an example where the optimizer is used for curve fitting.

## Points of interest in this implementation

The focus was to use types to catch implementation errors already while compiling,
or in other words making illegal states or calls impossible.
Look at the types Point, Evaluated, Unsorted and Sorted to understand, how that design goal
was tried to be achieved.

## References

- Optimization algorithm is Downhill-Simplex as described on [Wikipedia][wiki-simplex]
- Integrationtest uses method of least squares as cost function as described on [Wikipedia][wiki-kq]

[wiki-simplex]: https://de.wikipedia.org/wiki/Downhill-Simplex-Verfahren
[wiki-kq]: https://de.wikipedia.org/wiki/Methode_der_kleinsten_Quadrate
