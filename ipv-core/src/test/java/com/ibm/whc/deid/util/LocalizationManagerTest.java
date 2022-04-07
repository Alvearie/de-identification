/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.FileNotFoundException;
import java.util.Collection;
import java.util.Collections;
import java.util.Properties;
import org.junit.Before;
import org.junit.Test;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;

public class LocalizationManagerTest {
  private LocalizationManager manager;

  @Before
  public void setUp() {
    manager = LocalizationManager.getInstance(LocalizationManager.DEFAULT_LOCALIZATION_PROPERTIES);
  }

  @Test
  public void initialization() throws Exception {
    assertTrue(true);
  }

  @Test
  public void getAllResources() throws Exception {
    for (Resources resource : Resource.values()) {
      if (resource != Resource.PATTERN) {
        Collection<ResourceEntry> resources = manager.getResources(resource);

        assertNotNull(resources);
        assertNotEquals(0, resources.size());
      }
    }
  }

  @Test
  public void getResourcesUsIsEn() throws Exception {
    Collection<ResourceEntry> resources =
        manager.getResources(Resource.COUNTRY, Collections.singleton("us"));

    assertNotNull(resources);
    assertEquals(1, resources.size());

    for (ResourceEntry resourceEntry : resources)
      assertEquals("en", resourceEntry.getCountryCode());
  }

  @Test
  public void getResourcesWithCountry() throws Exception {
    for (Resources resource : Resource.values()) {
      if (resource != Resource.PATTERN) {

        Collection<ResourceEntry> resources =
            manager.getResources(resource, Collections.singleton("us"));

        assertNotNull(resources);
        assertNotEquals("Resource  " + resource + " size should not be zero", 0, resources.size());
      }
    }
  }

  /** Test that locale properties are acquired by the getter */
  @Test
  public void getLocalProperties() throws Exception {
    Properties props = manager.getLocaleProperties("us");
    assertTrue(props.size() > 0);
  }

  /** Ensure that invalid countries return an empty properties list */
  @Test
  public void getLocalPropertiesInvalidCountry() throws Exception {
    Properties props = manager.getLocaleProperties("#$!");
    assertTrue(props.size() == 0);
  }

  /** Ensure that valid countries with no locale information give an empty properties file */
  @Test
  public void getLocalPropertiesCountryWithNoLocale() throws Exception {
    Properties props = manager.getLocaleProperties("uk");
    assertTrue(props.size() == 0);
  }

  @Test
  public void testFileNotFound() throws Exception {
    String filename = "/not.found.!@#$.properties";
    try {
      LocalizationManager.getInstance(filename);
      fail("expected exception");
    } catch (RuntimeException e) {
      assertTrue(e.getCause() instanceof FileNotFoundException);
      assertEquals(filename, e.getCause().getMessage());
    }
  }
}
