/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.api.stats;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.model.ReflectionModel;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Provide information about the interaction between users.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(name = "TotalInteractions", description = "Provide information of the interaction between users")
public class TotalInteractions extends ReflectionModel implements Model {

  /**
   * The identifier of the source user that start the interaction.
   */
  @Schema(description = "The identifier of the source user that start the interaction", example = "1")
  public String sourceId;

  /**
   * The identifier of the user that ends the interaction.
   */
  @Schema(description = "The identifier of the target user that ends the interaction", example = "2")
  public String targetId;

  /**
   * The difference, measured in seconds, between the time when the source user do
   * the first interaction with the target user and midnight, January 1, 1970 UTC.
   */
  @Schema(description = "The UTC epoch timestamp representing the time of the first interaction between the source and target user.", example = "1563930000", nullable = false)
  public Long firstTs;

  /**
   * The difference, measured in seconds, between the time when the source user do
   * the last interaction with the target user and midnight, January 1, 1970 UTC.
   */
  @Schema(description = "The UTC epoch timestamp representing the time of the last interaction between the source and target user.", example = "1563930000", nullable = false)
  public Long lastTs;

  /**
   * The number of times each of them has interacted.
   */
  @Schema(description = "The number of interaction between the users.", example = "39034", nullable = false)
  public Long total;

}
