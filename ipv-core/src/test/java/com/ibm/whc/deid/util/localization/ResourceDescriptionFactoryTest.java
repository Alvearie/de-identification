/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.localization;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;

import java.util.Collection;
import org.junit.Test;

public class ResourceDescriptionFactoryTest {
  @Test
  public void testGetAllResourceDescriptions() throws Exception {

    Collection<ResourceDescription> descriptions = ResourceDescriptionFactory.getDescriptions();
    assertNotNull(descriptions);
    assertFalse(descriptions.isEmpty());

    ResourceDescriptionFactory factory = new ResourceDescriptionFactory();
    assertNotNull(factory);
  }
}
