<?php

$limit = NULL;
if (isset($argv[1])) {
  $limit = intval($argv[1]);
}

function fgetcsv2($f) {
  $row = fgetcsv($f);
//  $row[1] = substr($row[1],0,7);
//  $row[2] = substr($row[1],0,7);
  return $row;
}

$f = fopen('parroquias.csv', 'r');
fgetcsv($f); //skip header

echo <<<TAG
module Parroquias
( parroquias ) where

type Point = (Rational, Rational)
type Polygon = [Point]

parroquias :: [(Integer, Polygon)]
parroquias = [

TAG;

$currentPolygonId = NULL;
$points = [];
$parroquiaId = NULL;

$tempcount = 0;

while ($row = fgetcsv2($f)) {
  if ($limit && ++$tempcount == $limit)
    break;

  if (count($row) != 4) {
    continue;
  }

  $parroquiaId = $row[0];
  
  if ($currentPolygonId !== $row[3]) {
    if (!is_null($currentPolygonId))
      echo "  (".$parroquiaId.",[".implode(',', $points)."]),\n";

    $currentPolygonId = $row[3];
    $points = [];
  }
  
  $points[] = '('.$row[1].','.$row[2].')';

}

echo "  (".$parroquiaId.",[".implode(',', $points)."])\n";

echo "  ]\n";

fclose($f);
