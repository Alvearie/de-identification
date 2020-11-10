/*
 * (C) Copyright IBM Corp. 2016,2020
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
 * @param <T> the type parameter
 */
public class LatLonDistance<K extends Location> {
  private final Collection<K> locations;

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
   * Instantiates a new Lat lon distance calculator.
   *
   * @param locationList the location list
   * @throws Exception the exception
   */
  public LatLonDistance(Collection<K> locationList) {
    locations = locationList;
  }

  /**
   * Find nearest k list.
   *
   * @param coordinates the coordinates
   * @param k the k
   * @return the list
   */
  public List<K> findNearestK(double[] coordinates, int k) {
    LatitudeLongitude current = new LatitudeLongitude(coordinates[0], coordinates[1]);

    // Find the euclidean distance between this point and others
    List<Tuple2<Double, K>> closest = new ArrayList<>();
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
      k = closest.size() - 1;
    }

    List<K> toReturn = new ArrayList<>();
    for (int i = 0; i < k; i++) {
      toReturn.add(closest.get(i)._2());
    }

    return toReturn;
  }

  public List<K> findNearestK(K l, int k) {
    double[] location = new double[2];
    location[0] = l.getLocation().getLatitude();
    location[1] = l.getLocation().getLongitude();

    return findNearestK(location, k);
  }
}
