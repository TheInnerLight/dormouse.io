---
title: "Dormouse"
author: Me
date: Jan 31, 2021
tags: [haskell, dormouse]
description: Dormouse Index
isTop: True
---

# Dormouse

Dormouse is a set of libraries designed to facilitate productive, type-safe and testable HTTP in Haskell.

## Projects

Dormouse currently consists of:

- [Dormouse-Uri](uri.html), a library for type-safe representations of Urls and Uris.
- [Dormouse-Client](client.html), a simple, type-safe and testable HTTP client.

## Objectives

Dormouse aims to be:

- **User Friendly**: Dormouse libraries attempt to map as naturally as possible to the underlying problem domain.  Fancy code is great but only if it assists in representing the underlying abstractions.
- **Strict:** Dormouse libraries aim to make it extremely hard to write incorrect code by type checking as many common error scenarios as possible.
- **Customisable & Realistic:** We recognise that not all APIs are standards compliant and weird things occur in the real world.  Default behaviour should be strict but the library should support customisation to handle interaction with the weird and wonderful real world of bizarre systems and legacy code.
- **Testable:** Dormouse is built from the ground up to make your job of testing any code that is built on top of it quick, straightforward and painless making it easy to keep projects reliable and well maintained for a long time to come.

## Licence

Dormouse is distributed under a standard BSD 3-Clause Licence.
