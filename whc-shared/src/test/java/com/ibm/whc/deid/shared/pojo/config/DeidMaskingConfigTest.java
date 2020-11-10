/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.shared.pojo.config;

import static org.hamcrest.CoreMatchers.is;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.stream.Collectors;
import org.junit.BeforeClass;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.shared.util.ConfigGenerator;

public class DeidMaskingConfigTest {

  private static final Logger log = LoggerFactory.getLogger(DeidMaskingConfigTest.class);
  private static ObjectMapper objectMapper;

  @BeforeClass
  public static void setup() {
    objectMapper = new ObjectMapper();
  }

  /**
   * Create a default deid masking configuration. Serialize to String, de-serialize to POJO, and
   * serialize again to make sure the two strings are the same.
   *
   * @throws Exception
   */
  @Test
  public void testSerializeDedeserialize() throws Exception {

    DeidMaskingConfig maskingConfig = (new ConfigGenerator()).getTestDeidConfig();

    String maskingConfigStr = objectMapper.writeValueAsString(maskingConfig);
    log.info(maskingConfigStr);

    DeidMaskingConfig maskingConfig2 =
        objectMapper.readValue(maskingConfigStr, DeidMaskingConfig.class);

    String maskingConfigStr2 = objectMapper.writeValueAsString(maskingConfig2);

    log.info(maskingConfigStr2);
    assertEquals(maskingConfigStr, maskingConfigStr2);
  }

  @Test
  public void testDeserailize() throws IOException {

    Path path = Paths.get("src/test/resources/deid_masking_config.json");

    String maskingConfigStr = Files.readAllLines(path).stream().collect(Collectors.joining());

    DeidMaskingConfig maskingConfig =
        objectMapper.readValue(maskingConfigStr, DeidMaskingConfig.class);

    assertThat(maskingConfig.getRules().size(), is(3));
    assertThat(maskingConfig.getJson().getMaskingRules().size(), is(858));
  }
}
