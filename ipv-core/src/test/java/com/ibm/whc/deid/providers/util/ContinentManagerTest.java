/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.util;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.HashMap;
import org.junit.Test;
import com.ibm.whc.deid.models.Continent;
import com.ibm.whc.deid.util.ContinentManager;
import com.ibm.whc.deid.util.localization.LocalizationManager;

public class ContinentManagerTest {

  private String localizationProperty = LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES;

  @Test
  public void testGetClosest() throws Exception {
    ContinentManager manager = ContinentManager.buildContinentManager(localizationProperty);
    
    Continent source = manager.getValue("europe");
    assertNotNull(source);
    
    Collection<Continent> values = manager.getValues(source.getNameCountryCode());
    assertNotNull(values);
    assertTrue(values.size() > 1);
    
    HashMap<Continent, Integer> possibles = new HashMap<>(values.size() * 2);
    for (Continent c : values) {
      if (c != source) {
        possibles.put(c, Integer.valueOf(0));
      }
    }

    for (int i = 0; i < 100; i++) {
      Continent selected = manager.getClosestContinent(source, 1000);
      Integer count = possibles.get(selected);
      assertNotNull(count);
      possibles.put(selected, Integer.valueOf(count.intValue() + 1));
    }

    //for (Entry<Continent, Integer> entry : possibles.entrySet()) {
    //  System.out.println(entry.getKey().getName() + "=" + entry.getValue());
    //}
  }

  @Test
  public void testGetClosest_badK() throws Exception {
    ContinentManager manager = ContinentManager.buildContinentManager(localizationProperty);
    Continent source = manager.getValue("europe");
    assertNotNull(source);
    assertNotNull(manager.getClosestContinent(source, 0));
    assertNotNull(manager.getClosestContinent(source, -1));
  }
}
