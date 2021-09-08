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

import eu.internetofus.common.components.models.Message;
import eu.internetofus.common.components.models.TaskTransaction;
import eu.internetofus.common.model.Model;
import eu.internetofus.common.model.ReflectionModel;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Provide information about the interaction between users.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(name = "InteractionBetweenUsers", description = "Provide information of the interaction between users")
public class InteractionBetweenUsers extends ReflectionModel implements Model {

  /**
   * The transaction that has started the interaction.
   */
  @Schema(description = "The transaction that has started the interaction.")
  public TaskTransaction transaction;

  /**
   * The message that has ends the interaction.
   */
  @Schema(description = "The message that has ends the interaction.")
  public Message message;

  /**
   * The difference, measured in seconds, between the time when the interaction
   * has been done and midnight, January 1, 1970 UTC.
   */
  @Schema(description = "The UTC epoch timestamp representing the time when the interaction has been done.", example = "1563930000", nullable = false)
  public Long interactionTs;

}
