/*
 * (C) Copyright IBM Corp. 2016,2021
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.providers.masking.AbstractComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.ComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.MaskingProviderFactory;
import com.ibm.whc.deid.shared.exception.KeyedIllegalArgumentException;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;
import scala.Tuple2;

public class FHIRMaskingProvider extends AbstractComplexMaskingProvider
    implements ComplexMaskingProvider {

  private static final long serialVersionUID = 5945527984023679481L;

  public FHIRMaskingProvider(DeidMaskingConfig maskingConfiguration,
      MaskingProviderFactory maskingProviderFactory, String tenantId) {
    this(maskingConfiguration, maskingConfiguration.getCertificateId(),
        maskingConfiguration.isDefaultNoRuleResolution(), maskingProviderFactory, "/fhir/",
        tenantId);
  }

  protected FHIRMaskingProvider(DeidMaskingConfig maskingConfiguration, String certificateId,
      boolean defNoRuleRes, MaskingProviderFactory maskingProviderFactory, String basePathPrefix,
      String tenantId) {
    super(maskingConfiguration);

    this.maskingProviderFactory = maskingProviderFactory;
    this.certificateId = certificateId;

    // if using "default" message type, given message types list is ignored per documentation
    List<String> messageTypes;
    if (AbstractComplexMaskingProvider.DISABLE_TYPES_VALUE.equals(this.keyForType)) {
      messageTypes = new ArrayList<>();
      messageTypes.add(AbstractComplexMaskingProvider.DISABLE_TYPES_VALUE);
    } else {
      messageTypes = maskingConfiguration.getJson().getMessageTypes();
    }

    messageTypes.forEach(type -> {
      String basePath = basePathPrefix + type;

      List<FHIRResourceField> ruleAssignments =
          loadRulesForResource(type, maskingConfiguration, basePathPrefix);
      FHIRResourceMaskingConfiguration resourceConfiguration =
          new FHIRResourceMaskingConfiguration(basePath, ruleAssignments);
      this.maskingProviderMap.put(type.toLowerCase(),
          new MaskingProviderBuilder(resourceConfiguration, maskingConfiguration, defNoRuleRes,
              maskingProviderFactory, tenantId));
    });
  }

  /**
   * Load the rule assignments applicable for the given message type.
   *
   * @param resourceName the message type for which rules are obtained
   * @param maskingConfiguration the masking configuration
   * 
   * @return the applicable rule assignments
   */
  public static List<FHIRResourceField> loadRulesForResource(String resourceName,
      DeidMaskingConfig maskingConfiguration, String bashPathPrefix) {
    String matchPrefix = bashPathPrefix + resourceName + "/";
    List<FHIRResourceField> values = Collections.emptyList();
    JsonConfig jsonConfig = maskingConfiguration.getJson();
    if (jsonConfig != null) {
      List<JsonMaskingRule> ruleAssignments = jsonConfig.getMaskingRules();
      if (ruleAssignments != null && !ruleAssignments.isEmpty()) {
        values = new ArrayList<>(ruleAssignments.size());
        for (JsonMaskingRule assignment : ruleAssignments) {
          if (assignment.getJsonPath().startsWith(matchPrefix)) {
            values.add(new FHIRResourceField(assignment.getJsonPath(), assignment.getRule()));
          }
        }
      }
    }
    return values;
  }

  /** Given an identifier return the masked resource */
  @Override
  public String mask(String identifier) {
    throw new RuntimeException(
        "FHIRMaskingProvider: Masking on a per-string basis is not enabled.");
  }

  @Override
  public List<ReferableData> maskWithBatch(List<ReferableData> payloadData, String jobId) {
    List<Tuple2<String, JsonNode>> toMask = payloadData.stream().map(input -> {
      JsonNode node = null;
      try {
        node = ObjectMapperFactory.getObjectMapper().readTree(input.getData());
      } catch (JsonProcessingException e) {
        throw new KeyedIllegalArgumentException(LogCodes.WPH1026E,
            Messages.getMessage(LogCodes.WPH1026E, input.getIdentifier(), e.getMessage()), e);
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
        // this is unlikely to occur
        throw new KeyedRuntimeException(LogCodes.WPH1013E,
            Messages.getMessage(LogCodes.WPH1013E, e.getMessage()), e);
      }
      return serializedNode;
    }).collect(Collectors.toList());
    return toReturn;
  }

  @Override
  public void maskIdentifierBatch(List<MaskingActionInputIdentifier> identifiers) {
    // no action required here
  }
}
