/*
 * © Merative US L.P. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.etl.objects;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import com.ibm.whc.deid.util.Tuple;
import org.junit.Before;
import org.junit.Test;

public class FilterTest {
  private Filter filter;
  private static final String PATTERN = "found";

  @Before
  public void setUp() throws Exception {
    filter = new Filter(PATTERN);
  }

  @Test
  public void testApplyFound() throws Exception {
    Tuple<String, Boolean> tuple = filter.apply("found");
    assertNotNull(tuple);
    assertNull(tuple.getFirst());
    assertTrue(tuple.getSecond());
  }

  @Test
  public void testApplyNotFound() throws Exception {
    Tuple<String, Boolean> tuple = filter.apply("notfound");
    assertNotNull(tuple);
    assertNull(tuple.getFirst());
    assertFalse(tuple.getSecond());
  }
}
