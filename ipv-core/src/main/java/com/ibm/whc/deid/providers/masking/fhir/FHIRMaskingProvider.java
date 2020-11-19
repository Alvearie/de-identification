/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.configuration.MaskingConfiguration;
import com.ibm.whc.deid.providers.masking.AbstractComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.MaskingProviderFactory;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import com.ibm.whc.deid.utils.log.LogCodes;
import scala.Tuple2;

public class FHIRMaskingProvider extends AbstractComplexMaskingProvider<String> {
  /** */
  private static final long serialVersionUID = 5945527984023679481L;

  private List<String> copyFieldList = new ArrayList<String>();

  public FHIRMaskingProvider(DeidMaskingConfig maskingConfiguration,
      MaskingProviderFactory maskingProviderFactory, String tenantId) {
    this(maskingConfiguration, maskingConfiguration.getCertificateID(), true,
        true, maskingProviderFactory, "/fhir/", tenantId);
  }

  protected FHIRMaskingProvider(DeidMaskingConfig maskingConfiguration,
      String certificateID, boolean arrayAllRules, boolean defNoRuleRes,
      MaskingProviderFactory maskingProviderFactory, String basePathPrefix, String tenantId) {
    super(maskingConfiguration);

    this.maskingProviderFactory = maskingProviderFactory;
    this.certificateID = certificateID;

    List<String> messageTypes = maskingConfiguration.getJson().getMessageTypes();
    messageTypes.forEach(type -> {
      String basePath = basePathPrefix + type;

      Map<String, String> maskingConfigurations =
          loadRulesForResource(type, maskingConfiguration, basePathPrefix);
      FHIRResourceMaskingConfiguration resourceConfiguration =
          new FHIRResourceMaskingConfiguration(basePath, maskingConfigurations);
      this.maskingProviderMap.put(type.toLowerCase(),
          new MaskingProviderBuilder("fhir", resourceConfiguration, maskingConfiguration,
              arrayAllRules, defNoRuleRes, maskingProviderFactory, tenantId));
    });
  }

  /**
   * Given a masking configuration, parses and appends "/fhir/maskingconf" to the resource name
   *
   * @param resourceName
   * @param maskingConfiguration
   * @return
   */
  public static Map<String, String> loadRulesForResource(String resourceName,
      MaskingConfiguration maskingConfiguration, String prefix) {
    return maskingConfiguration.getStringValueWithPrefixMatch(prefix + resourceName + "/");
  }

  /**
   * Given a masking configuration, load rules with given resource name.
   *
   * @param resourceName
   * @param maskingConfiguration
   * @return
   */
  public static Map<String, String> loadRulesForResource(String resourceName,
      DeidMaskingConfig maskingConfiguration, String bashPathPrefix) {
    return maskingConfiguration.getStringValueWithPrefixMatch(bashPathPrefix + resourceName + "/");
  }

  /** Given an identifier return the masked resource */
  public String mask(String identifier) {
    throw new RuntimeException(
        "FHIRMaskingProvider: Masking on a per-string basis is not enabled.");
  }

  public List<String> getCopyFieldList() {
    return copyFieldList;
  }

  public void setCopyFieldList(List<String> copyFieldList) {
    this.copyFieldList = copyFieldList;
  }

  public List<ReferableData> maskWithBatch(List<ReferableData> payloadData,
      String jobId) {
    List<Tuple2<String, JsonNode>> toMask = payloadData.stream().map(input -> {
      JsonNode node = null;
      try {
        node = ObjectMapperFactory.getObjectMapper().readTree(input.getData());
      } catch (JsonProcessingException e) {
        log.logError(LogCodes.WPH1013E, e);
      } catch (IOException e) {
        log.logError(LogCodes.WPH1013E, e);
      }
      return new Tuple2<String, JsonNode>(input.getIdentifier(), node);
    }).collect(Collectors.toList());
    toMask = maskJsonNode(toMask);
    List<ReferableData> toReturn = toMask.stream().map(input -> {
      ReferableData serializedNode = null;
      try {
        serializedNode = new ReferableData(input._1(),
            ObjectMapperFactory.getObjectMapper().writeValueAsString(input._2()));
      } catch (JsonProcessingException e) {
        log.logError(LogCodes.WPH1013E, e);
      }
      return serializedNode;
    }).collect(Collectors.toList());
    return toReturn;
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    // TODO Auto-generated method stub

  }
}
