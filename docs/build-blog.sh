#!/usr/bin/env bash
pandoc blog.md -o index.html --standalone --lua-filter=pandoc-filters.lua --syntax-highlighting=kate
