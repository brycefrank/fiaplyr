---
title: "Path to packaged Vermont mini FIADB DuckDB"
description: "Returns the installed path to the DuckDB file distributed with ‘fiaplyr’."
---

## Description

Returns the installed path to the DuckDB file distributed with
‘fiaplyr’.

## Usage

```r
fiadb_vt_mini_path(mustWork = TRUE)
```

## Arguments

- `mustWork`: Logical. If TRUE (default), throws an error when the file is not found.

## Value

A length-1 character vector containing the file path.
