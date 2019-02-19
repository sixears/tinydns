{ pkgs ? import <nixpkgs> {} }:

with pkgs;
{ inherit djbdns; }
