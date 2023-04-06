/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.util.localization;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.util.Collections;
import org.junit.Test;
import com.ibm.whc.deid.shared.localization.Resource;
import com.ibm.whc.deid.shared.localization.Resources;

public class ResourceDescriptionTest {
  private static final Resources RESOURCE = Resource.CITY;
  private static final ResourceFormat RESOURCE_FORMAT = ResourceFormat.CSV;
  private static final String DESCRIPTION = "description";

  @Test
  public void getMethods() throws Exception {

    ResourceDescription description = new ResourceDescription(RESOURCE, RESOURCE_FORMAT, null,
        DESCRIPTION, Collections.<String>emptyList());
    assertEquals("getResource", RESOURCE, description.getResource());
    assertEquals("getFormat", RESOURCE_FORMAT, description.getFormat());
    assertEquals("getDescription", DESCRIPTION, description.getDescription());
    assertNotNull("getFieldDescriptions", description.getFieldDescriptions());

    ResourceDescriptionFactory factory = new ResourceDescriptionFactory();
    assertNotNull(factory);
  }
}
