/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import com.ibm.whc.deid.models.LatitudeLongitude;
import com.ibm.whc.deid.models.Location;
import scala.Tuple2;

/**
 * A calculator for distances between points expressed as latitude and longitude coordinates.
 */
public class LatLonDistance<K extends Location> {

  private final Collection<K> locations;

  /**
   * Instantiates a new distance calculator.
   *
   * @param locationList the list of possible locations
   */
  public LatLonDistance(Collection<K> locationList) {
    locations = locationList;
  }

  /**
   * Finds the euclidean distance between two points
   * 
   * @param point1 the first point
   * @param point2 the second point
   * @return the distance
   */
  private Double euclidean(LatitudeLongitude point1, LatitudeLongitude point2) {
    return Math.sqrt(Math.pow(point1.getLatitude() - point2.getLatitude(), 2)
        + Math.pow(point1.getLongitude() - point2.getLongitude(), 2));
  }

  /**
   * Find nearest locations to a given starting location.
   *
   * @param latitude latitude of starting location
   * @param longitude longitude of starting location
   * @param k the number of nearby locations to include in the list
   * 
   * @return the non-null list of nearby locations
   */
  public List<K> findNearestK(double latitude, double longitude, int k) {

    LatitudeLongitude current = new LatitudeLongitude(latitude, longitude);

    // Find the euclidean distance between this point and others
    List<Tuple2<Double, K>> closest = new ArrayList<>(locations.size());
    for (K l : locations) {
      Double result = euclidean(l.getLocation(), current);
      closest.add(new Tuple2<Double, K>(result, l));
    }

    // Sort the list
    closest.sort((element1, element2) -> {
      return element1._1().compareTo(element2._1());
    });

    closest.removeIf(i -> (i._1().equals(0.0)));

    // Return the first k records
    if (k > closest.size()) {
      k = closest.size();
    }
    if (k < 0) {
      k = 0;
    }
    List<K> toReturn = new ArrayList<>(k);
    for (int i = 0; i < k; i++) {
      toReturn.add(closest.get(i)._2());
    }

    return toReturn;
  }

  public List<K> findNearestK(K l, int k) {
    return findNearestK(l.getLocation().getLatitude(), l.getLocation().getLongitude(), k);
  }
}
