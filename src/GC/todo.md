# Garbage collection

## Project

Goal for next week (24/2):
- Write more complex tests

## GC TODO:
- Skriva klart profiler
    - fixa abs_path i create_file_stream
- delete chunk i free_overlap
- Kolla linking med Valter/Victor
- Fixa en a-fil/static lib till Samuel
- Kolla vektor vs list complexity
- Se om det är bättre att lagra Chunk och inte Chunk* i data strukturerna, 
då är alla efter varandra i minnet.

## Tests TODO
- Write complex datastructures for tests with larger programs

## Profiler grejer
1. [x] Kolla existerande events
2. [x] Nya events för init, dispose i heap.cpp
3. [x] Skriv om try_recycle för att returnera en chunk
4. [x] Copy constructor för chunks (reused chunks)
5. [x] Nya events för try_recycle