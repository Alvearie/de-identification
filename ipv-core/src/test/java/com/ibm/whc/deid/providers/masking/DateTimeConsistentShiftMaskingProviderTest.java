/*
 * (C) Copyright IBM Corp. 2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig;
import com.ibm.whc.deid.shared.pojo.config.masking.DateTimeConsistentShiftMaskingProviderConfig.DateShiftDirection;

public class DateTimeConsistentShiftMaskingProviderTest implements MaskingProviderTest {

  private class TestDateTimeConsistentShiftMaskingProvider
      extends DateTimeConsistentShiftMaskingProvider {

    private static final long serialVersionUID = 1L;

    public long longFromSeed;

    public TestDateTimeConsistentShiftMaskingProvider(
        DateTimeConsistentShiftMaskingProviderConfig configuration) {
      super(configuration);
    }

    @Override
    protected long generateLongFromString(String seed) {
      return longFromSeed;
    }
  }

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

  @Test
  public void testGenerateShiftNumberOfDays_before() {
    DateTimeConsistentShiftMaskingProviderConfig config =
        new DateTimeConsistentShiftMaskingProviderConfig();
    config.setPatientIdentifierPath("/id");
    config.setDateShiftDirection(DateShiftDirection.before);
    config.setDateShiftMinimumDays(1);
    config.setDateShiftMaximumDays(10);

    TestDateTimeConsistentShiftMaskingProvider provider =
        new TestDateTimeConsistentShiftMaskingProvider(config);

    int target = -10;
    for (provider.longFromSeed = 0L; provider.longFromSeed < 30L; provider.longFromSeed++) {
      assertEquals(target, provider.generateShiftNumberOfDays(""));
      target = target == -1 ? -10 : target + 1;
    }
    
    provider.longFromSeed = 1000000003L;
    assertEquals(-7, provider.generateShiftNumberOfDays(""));

    config.setDateShiftMinimumDays(0);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    target = -10;
    for (provider.longFromSeed = 0L; provider.longFromSeed < 33L; provider.longFromSeed++) {
      assertEquals(target, provider.generateShiftNumberOfDays(""));
      target = target == 0 ? -10 : target + 1;
    }

    config.setDateShiftMaximumDays(0);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    for (provider.longFromSeed = 0L; provider.longFromSeed < 20L; provider.longFromSeed++) {
      assertEquals(0, provider.generateShiftNumberOfDays(""));
    }

    config.setDateShiftMinimumDays(2);
    config.setDateShiftMaximumDays(2);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    for (provider.longFromSeed = 0L; provider.longFromSeed < 20L; provider.longFromSeed++) {
      assertEquals(-2, provider.generateShiftNumberOfDays(""));
    }
  }

  @Test
  public void testGenerateShiftNumberOfDays_after() {
    DateTimeConsistentShiftMaskingProviderConfig config =
        new DateTimeConsistentShiftMaskingProviderConfig();
    config.setPatientIdentifierPath("/id");
    config.setDateShiftDirection(DateShiftDirection.after);
    config.setDateShiftMinimumDays(1);
    config.setDateShiftMaximumDays(10);

    TestDateTimeConsistentShiftMaskingProvider provider =
        new TestDateTimeConsistentShiftMaskingProvider(config);

    int target = 1;
    for (provider.longFromSeed = 0L; provider.longFromSeed < 30L; provider.longFromSeed++) {
      assertEquals(target, provider.generateShiftNumberOfDays(""));
      target = target == 10 ? 1 : target + 1;
    }

    provider.longFromSeed = 1000000003L;
    assertEquals(4, provider.generateShiftNumberOfDays(""));

    config.setDateShiftMinimumDays(0);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    target = 0;
    for (provider.longFromSeed = 0L; provider.longFromSeed < 33L; provider.longFromSeed++) {
      assertEquals(target, provider.generateShiftNumberOfDays(""));
      target = target == 10 ? 0 : target + 1;
    }

    config.setDateShiftMaximumDays(0);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    for (provider.longFromSeed = 0L; provider.longFromSeed < 20L; provider.longFromSeed++) {
      assertEquals(0, provider.generateShiftNumberOfDays(""));
    }

    config.setDateShiftMinimumDays(3);
    config.setDateShiftMaximumDays(3);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    for (provider.longFromSeed = 0L; provider.longFromSeed < 20L; provider.longFromSeed++) {
      assertEquals(3, provider.generateShiftNumberOfDays(""));
    }
  }

  @Test
  public void testGenerateShiftNumberOfDays_beforeOrAfter() {
    DateTimeConsistentShiftMaskingProviderConfig config =
        new DateTimeConsistentShiftMaskingProviderConfig();
    config.setPatientIdentifierPath("/id");
    config.setDateShiftDirection(DateShiftDirection.beforeOrAfter);
    config.setDateShiftMinimumDays(1);
    config.setDateShiftMaximumDays(10);

    TestDateTimeConsistentShiftMaskingProvider provider =
        new TestDateTimeConsistentShiftMaskingProvider(config);

    int target = -10;
    for (provider.longFromSeed = 0L; provider.longFromSeed < 60L; provider.longFromSeed++) {
      assertEquals(target, provider.generateShiftNumberOfDays(""));
      if (target == 10) {
        target = -10;
      } else if (target == -1) {
        target = 1;
      } else {
        target++;
      }
    }

    provider.longFromSeed = 1000000003L;
    assertEquals(-7, provider.generateShiftNumberOfDays(""));
    provider.longFromSeed = 1000000000013L;
    assertEquals(4, provider.generateShiftNumberOfDays(""));

    config.setDateShiftMinimumDays(0);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    target = -10;
    for (provider.longFromSeed = 0L; provider.longFromSeed < 63L; provider.longFromSeed++) {
      assertEquals(target, provider.generateShiftNumberOfDays(""));
      target = target == 10 ? -10 : target + 1;
    }

    config.setDateShiftMaximumDays(0);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    for (provider.longFromSeed = 0L; provider.longFromSeed < 20L; provider.longFromSeed++) {
      assertEquals(0, provider.generateShiftNumberOfDays(""));
    }

    config.setDateShiftMinimumDays(3);
    config.setDateShiftMaximumDays(3);
    provider = new TestDateTimeConsistentShiftMaskingProvider(config);

    target = -3;
    for (provider.longFromSeed = 0L; provider.longFromSeed < 20L; provider.longFromSeed++) {
      assertEquals(target, provider.generateShiftNumberOfDays(""));
      target *= -1;
    }
  }
}
