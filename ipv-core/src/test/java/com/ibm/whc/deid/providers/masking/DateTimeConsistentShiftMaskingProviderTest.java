/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig;

public class DateTimeConsistentShiftMaskingProviderTest implements MaskingProviderTest {

  @Test
  public void testApplyOffsetAndReformat() {
    DateTimeConsistentShiftMaskingProviderConfig config =
        new DateTimeConsistentShiftMaskingProviderConfig();
    config.setPatientIdentifierPath("/id");
    DateTimeConsistentShiftMaskingProvider provider =
        new DateTimeConsistentShiftMaskingProvider(config);

    assertEquals("2020-02-29", provider.applyOffsetAndReformat("2020-03-01", -1));
    assertEquals("2020-02-29 13:14:15", provider.applyOffsetAndReformat("2020-03-01 13:14:15", -1));
    assertEquals("2020-02-29T01:02:03-01:00",
        provider.applyOffsetAndReformat("2020-03-01T01:02:03-01:00", -1));
    assertEquals("2020-02-29T01:02:03Z",
        provider.applyOffsetAndReformat("2020-03-01T01:02:03Z", -1));
    assertEquals("2020-10-31T05:06:07-05:00",
        provider.applyOffsetAndReformat("2020-11-01T05:06:07-05:00", -1));
    assertEquals("2020-03-08T02:03:04-05:00",
        provider.applyOffsetAndReformat("2020-03-09T02:03:04-05:00", -1));
    assertEquals("2020-02-29T01:02:03-01:00",
        provider.applyOffsetAndReformat("2020-03-01T01:02:03-01:00", -1));
    assertEquals("2020-02-29T01:02:03-01:00",
        provider.applyOffsetAndReformat("2020-03-01T01:02:03-01:00", -1));
    assertEquals("2020-02-29T01:02:03-01:00",
        provider.applyOffsetAndReformat("2020-03-01T01:02:03-01:00", -1));

    assertEquals("2020-01-03", provider.applyOffsetAndReformat("2020-01-03", 0));
    assertEquals("2019-12-24", provider.applyOffsetAndReformat("2020-01-03", -10));
    assertEquals("2019-12-31", provider.applyOffsetAndReformat("2020-03-01", -61));
    assertEquals("2020-01-03", provider.applyOffsetAndReformat("2019-12-24", 10));
    assertEquals("2020-03-01", provider.applyOffsetAndReformat("2019-12-31", 61));

    assertEquals("2019-12-29T01:02:03+03:00",
        provider.applyOffsetAndReformat("2020-03-01T01:02:03+03:00", -63));

    // TODO: add?
    // "2020-11-01T05:06:07-05:00[America/Chicago]"
    // "2020-03-09T02:03:04-05:00[America/Chicago]"
  }
}
