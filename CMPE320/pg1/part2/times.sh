#!/bin/bash

for i in {1..10}; do
  for j in {1..10}; do

      k=`expr $i \* $j`;

     printf "$k ";

 done

 printf "\n";

done
