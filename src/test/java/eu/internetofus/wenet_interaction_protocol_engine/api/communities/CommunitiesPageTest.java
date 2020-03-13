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

import java.util.ArrayList;

import eu.internetofus.common.api.models.ModelTestCase;

/**
 * Test the {@link CommunitiesPage}.
 *
 * @see CommunitiesPage
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class CommunitiesPageTest extends ModelTestCase<CommunitiesPage> {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public CommunitiesPage createModelExample(int index) {

		final CommunitiesPage model = new CommunitiesPage();
		model.offset = index;
		model.total = 100 + index;
		model.communities = new ArrayList<>();
		model.communities.add(new CommunityTest().createModelExample(index));
		return model;
	}

}