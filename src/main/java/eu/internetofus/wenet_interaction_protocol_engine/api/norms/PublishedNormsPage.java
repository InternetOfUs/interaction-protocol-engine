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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import java.util.List;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.model.ReflectionModel;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Contains the found published norms.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(description = "Contains a set of published norms found")
public class PublishedNormsPage extends ReflectionModel implements Model {

  /**
   * The index of the first community returned.
   */
  @Schema(description = "The index of the first published norm returned.", example = "0")
  public int offset;

  /**
   * The number total of communities that satisfies the search.
   */
  @Schema(description = "The number total of published norms that satisfies the search.", example = "100")
  public long total;

  /**
   * The published norms.
   */
  @ArraySchema(schema = @Schema(implementation = PublishedNorm.class), arraySchema = @Schema(description = "The set of published norms found"))
  public List<PublishedNorm> norms;

}
