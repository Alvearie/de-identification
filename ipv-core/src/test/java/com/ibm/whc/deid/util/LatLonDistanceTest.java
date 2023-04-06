/*
 * © Merative US L.P. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import org.junit.Test;
import com.ibm.whc.deid.models.City;
import com.ibm.whc.deid.models.LatitudeLongitude;

public class LatLonDistanceTest {

  @Test
  public void testLatDistance() throws Exception {

    List<City> locationList = new ArrayList<>();

    City c1 = new City("IBM Campus", 53.4184439, -6.4165875, "en");
    City c2 = new City("The Mayne, Clonee", 53.422235, -6.426072, "en");
    City cx1 = new City("unknown1, Clonee", null, "-6.426072", "en");
    City cx2 = new City("unknown2, Clonee", "53.422235", "", "en");
    City c3 = new City("Damastown Industrial Park", 53.414601, -6.412983, "en");
    City c4 = new City("Carlton Hotel Tyrrelstown", 53.419480, -6.379337, "en");

    locationList.add(c1);
    locationList.add(c2);
    locationList.add(cx1);
    locationList.add(cx2);
    locationList.add(c3);
    locationList.add(c4);

    LatLonDistance<City> tree = new LatLonDistance<>(locationList);

    List<City> neighbors = tree.findNearestK(new LatitudeLongitude(41.6686, -6.416673), 55);
    assertEquals(4, neighbors.size());

    neighbors = tree.findNearestK(new LatitudeLongitude(53.416686, -6.416673), 2);

    Collections.sort(neighbors, new Comparator<City>() {
      @Override
      public int compare(City o1, City o2) {
        return o1.getName().compareTo(o2.getName());
      }
    });

    assertTrue(neighbors.get(0).getName().equals("Damastown Industrial Park"));
    assertTrue(neighbors.get(1).getName().equals("IBM Campus"));
  }

  @Test
  public void testLatDistance_none() throws Exception {

    List<City> locationList = new ArrayList<>();

    City c1 = new City("IBM Campus", "  ", "-6.4165875", "en");
    City c2 = new City("The Mayne, Clonee", "53.422235", null, "en");
    City c3 = new City("Damastown Industrial Park", null, "-6.412983", "en");
    City c4 = new City("Carlton Hotel Tyrrelstown", "53.419480", "", "en");

    locationList.add(c1);
    locationList.add(c2);
    locationList.add(c3);
    locationList.add(c4);

    LatLonDistance<City> tree = new LatLonDistance<>(locationList);

    List<City> neighbors = tree.findNearestK(new LatitudeLongitude(41.6686, -6.416673), 55);
    assertEquals(0, neighbors.size());
  }
}
