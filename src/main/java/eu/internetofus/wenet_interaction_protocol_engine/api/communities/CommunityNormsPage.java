/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */

package eu.internetofus.wenet_interaction_protocol_engine.api.communities;

import java.util.List;

import eu.internetofus.wenet_interaction_protocol_engine.Model;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * Page of community norms obtained after a search.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(description = "Contains some community norms that have been found")
public class CommunityNormsPage extends Model {

	/**
	 * The index of the first community returned.
	 */
	@Schema(description = "The index of the first community norm returned.", example = "0")
	public int offset;

	/**
	 * The number total of communities that satisfies the search.
	 */
	@Schema(description = "The number total of community norms that satisfies the search.", example = "100")
	public long total;

	/**
	 * The identifier of the user.
	 */
	@ArraySchema(
			schema = @Schema(implementation = Community.class),
			arraySchema = @Schema(description = "The set of community norms found"))
	public List<CommunityNorm> norms;
}
