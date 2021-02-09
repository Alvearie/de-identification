/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.Matchers.not;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.Collections;
import java.util.Properties;
import org.junit.Before;
import org.junit.Test;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.util.localization.LocalizationManager;
import com.ibm.whc.deid.util.localization.ResourceEntry;

public class LocalizationManagerTest {
  private LocalizationManager manager;

  @Before
  public void setUp() {
    manager = LocalizationManager.getInstance();
  }

  @Test
  public void initialization() throws Exception {
    assertTrue(true);
  }

  @Test
  public void getAllResources() throws Exception {
    for (Resource resource : Resource.values()) {
      if (resource != Resource.PATTERN) {
        Collection<ResourceEntry> resources = manager.getResources(resource);

        assertNotNull(resources);
        assertThat(resources.size(), is(not(0)));
      }
    }
  }

  @Test
  public void getResourcesUsIsEn() throws Exception {
    Collection<ResourceEntry> resources =
        manager.getResources(Resource.COUNTRY, Collections.singleton("us"));

    assertNotNull(resources);
    assertThat(resources.size(), is(1));

    for (ResourceEntry resourceEntry : resources)
      assertThat(resourceEntry.getCountryCode(), is("en"));
  }

  @Test
  public void getResourcesUkIsEn() throws Exception {
    Collection<ResourceEntry> resources =
        manager.getResources(Resource.COUNTRY, Collections.singleton("uk"));

    assertNotNull(resources);
    assertThat(resources.size(), is(1));

    for (ResourceEntry resourceEntry : resources)
      assertThat(resourceEntry.getCountryCode(), is("en"));
  }

  @Test
  public void getResourcesWithCountry() throws Exception {
    for (Resource resource : Resource.values()) {
      if (resource != Resource.PATTERN) {
        Collection<ResourceEntry> resources =
            manager.getResources(resource, Collections.singleton("us"));

        assertNotNull(resources);
				assertThat("Resource  " + resource + " size should not be zero", resources.size(),
						is(not(0)));
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
}
