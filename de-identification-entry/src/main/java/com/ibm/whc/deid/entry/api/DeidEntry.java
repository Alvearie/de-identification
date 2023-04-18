/*
 * © Merative US L.P. 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.entry.api;

import java.io.IOException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.entry.api.impl.DeidProcessorImpl;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.util.InvalidMaskingConfigurationException;

/**
 * Entry point for De-ID service java interface. Used when De-ID capabilities are being provided as
 * a local Java utility jar.
 */
public class DeidEntry {

  private static final DeidEntry instance_ = new DeidEntry();

  /**
   * Creates a De-ID interface with the given configuration that can be used to process any number
   * of input documents.
   * 
   * @param maskingConfiguration the masking configuration as a string in JSON format
   * 
   * @return an interface to the configured data masking engine
   * 
   * @throws IOException if the given strings cannot be parsed into the expected JSON structures
   * @throws InvalidMaskingConfigurationException if the given masking configuration contains
   *         invalid values or combinations of values
   * @throws RuntimeException if the given configuration contains invalid values or combinations of
   *         values
   */
  public static DeidProcessor getDeidProcessor(final String maskingConfiguration)
      throws IOException, InvalidMaskingConfigurationException {
    return instance_.buildDeidProcessor(maskingConfiguration);
  }

  protected DeidEntry() {
    // nothing required here
  }

  /**
   * @see #getDeidProcessor(String)
   */
  protected DeidProcessor buildDeidProcessor(final String maskingConfiguration)
      throws IOException, InvalidMaskingConfigurationException {
    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();
    DeidMaskingConfig maskingConfig =
        (maskingConfiguration == null || maskingConfiguration.trim().isEmpty()) ? null
            : objectMapper.readValue(maskingConfiguration, DeidMaskingConfig.class);
    return buildDeidProcessor(maskingConfig);
  }

  /**
   * Creates a De-ID interface with the given configuration that can be used to process any number
   * of input documents.
   * 
   * @param maskingConfiguration the masking configuration
   * 
   * @return an interface to the configured data masking engine
   * 
   * @throws IOException if the given strings cannot be parsed into the expected JSON structures
   * @throws InvalidMaskingConfigurationException if the given masking configuration contains
   *         invalid values or combinations of values
   * @throws RuntimeException if the given configuration contains invalid values or combinations of
   *         values
   */
  protected DeidProcessor buildDeidProcessor(final DeidMaskingConfig maskingConfiguration)
      throws InvalidMaskingConfigurationException {

    // Not having a public static version of this method reduces the risks of erroneous in-flight
    // changes to the configuration objects after de-identification processing has been initialized.

    return new DeidProcessorImpl(maskingConfiguration);
  }
}
