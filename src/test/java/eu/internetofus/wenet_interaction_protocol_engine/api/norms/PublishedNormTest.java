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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayList;
import java.util.UUID;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import eu.internetofus.common.TimeManager;
import eu.internetofus.common.api.models.ModelTestCase;
import eu.internetofus.common.api.models.ValidationErrorException;
import eu.internetofus.common.api.models.ValidationsTest;
import eu.internetofus.common.services.WeNetProfileManagerService;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link PublishedNorm}
 *
 * @see PublishedNorm
 *
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class PublishedNormTest extends ModelTestCase<PublishedNorm> {

	/**
	 * {@inheritDoc}
	 */
	@Override
	public PublishedNorm createModelExample(int index) {

		final PublishedNorm model = new PublishedNorm();
		model.name = "Published norm " + index;
		model.description = "Description of published norm " + index;
		model.keywords = new ArrayList<>();
		for (int i = index - 2; i < index + 2; i++) {

			model.keywords.add("keyword " + i);
		}
		model.publisherId = "Published identifier " + index;
		model.publishTime = index * 100000;
		model.norm = new NormTest().createModelExample(index);

		return model;
	}

	/**
	 * Create a valid published norm.
	 *
	 * @param index          of the example to create.
	 * @param profileManager service to manage the profiles.
	 * @param createHandler  component that manage the creation result.
	 */
	public static void createValidPublishedNormExample(int index, WeNetProfileManagerService profileManager,
			Handler<AsyncResult<PublishedNorm>> createHandler) {

		profileManager.createProfile(new JsonObject(), create -> {

			if (create.failed()) {

				createHandler.handle(Future.failedFuture(create.cause()));

			} else {

				final PublishedNorm result = new PublishedNormTest().createModelExample(index);
				result.publisherId = create.result().getString("id");
				result.publishTime = TimeManager.now();
				createHandler.handle(Future.succeededFuture(result));
			}

		});

	}

	/**
	 * Check that an right published norm is valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldRightPublishedNormBeValid(WeNetProfileManagerService profileManager, VertxTestContext testContext) {

		createValidPublishedNormExample(43, profileManager, testContext.succeeding(model -> {
			model.validate("codePrefix", profileManager, testContext.succeeding(ignored -> {
				testContext.completeNow();

			}));
		}));
	}

	/**
	 * Check that an empty published norm is not valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldEmptyPublishedNormNotBeValid(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm model = new PublishedNorm();
		model.validate("codePrefix", profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.norm");
			testContext.completeNow();

		})));

	}

	/**
	 * Create a minimum published norm that will be valid.
	 *
	 * @return the created published norm.
	 */
	public static PublishedNorm createMinimumValidPublishedNormExample() {

		final PublishedNorm model = new PublishedNorm();
		model.norm = new Norm();
		return model;

	}

	/**
	 * Create the norm with the minimum fields to be valid.
	 *
	 * @return the created minimum
	 */

	/**
	 * Check that a published norm with only a norm is valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldPublishedNormWithOnlyAnormBeValid(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm model = createMinimumValidPublishedNormExample();
		model.validate("codePrefix", profileManager, testContext.succeeding(ignored -> {
			testContext.completeNow();

		}));

	}

	/**
	 * Check that a published norm with an identifier is not valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldPublishedNormWithIdNotBeValid(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm model = new PublishedNorm();
		model.norm = new NormTest().createModelExample(1);
		model._id = "Defined identifier";
		model.validate("codePrefix", profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix._id");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that a published norm with a large name is not valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldPublishedNormWithLargeNameNotBeValid(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm model = new PublishedNorm();
		model.norm = new NormTest().createModelExample(1);
		model.name = ValidationsTest.STRING_256;
		model.validate("codePrefix", profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.name");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that a published norm with a large description is not valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldPublishedNormWithLargeDescriptionNotBeValid(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm model = new PublishedNorm();
		model.norm = new NormTest().createModelExample(1);
		model.description = ValidationsTest.STRING_256;
		model.validate("codePrefix", profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.description");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that a published norm with a large keyword is not valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldPublishedNormWithLargeKeywordNotBeValid(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm model = new PublishedNorm();
		model.norm = new NormTest().createModelExample(1);
		model.keywords = new ArrayList<>();
		model.keywords.add("  ");
		model.keywords.add(null);
		model.keywords.add(ValidationsTest.STRING_256);
		model.validate("codePrefix", profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.keywords[2]");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that a published norm with a large norm attribute is not valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldPublishedNormWithLargeNormAttributeNotBeValid(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm model = new PublishedNorm();
		model.norm = new NormTest().createModelExample(1);
		model.norm.attribute = ValidationsTest.STRING_256;
		model.validate("codePrefix", profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.norm.attribute");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that a published norm with a publishedId that is not defined is not
	 * valid.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldPublishedNormWithAnUndefinedPublishedIdNotBeValid(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm model = new PublishedNorm();
		model.norm = new NormTest().createModelExample(1);
		model.publisherId = "Undefined published identifier";
		model.validate("codePrefix", profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.publisherId");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can merge with a {@code null} source.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldMergePublishedNormWithNullSource(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", null, profileManager, testContext.succeeding(merged -> testContext.verify(() -> {

			assertThat(merged).isSameAs(target);
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can merge with an empty source.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldMergePublishedNormWithEmptyModel(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source._id = UUID.randomUUID().toString();
		source.publishTime = 12345;
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", source, profileManager, testContext.succeeding(merged -> testContext.verify(() -> {

			assertThat(merged).isNotSameAs(target).isEqualTo(target);
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can merge the name.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldMergePublishedNormName(WeNetProfileManagerService profileManager, VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.name = "Source Name";
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.name = "Target Name";
		target.merge("codePrefix", source, profileManager, testContext.succeeding(merged -> testContext.verify(() -> {

			assertThat(merged).isNotSameAs(target).isNotEqualTo(target);
			target.name = "Source Name";
			assertThat(merged).isEqualTo(target);
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can not merge with a bad name
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldNotMergePublishedNormWithBadName(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.name = ValidationsTest.STRING_256;
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", source, profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.name");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can merge the description.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldMergePublishedNormDescription(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.description = "Source Description";
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.description = "Target Description";
		target.merge("codePrefix", source, profileManager, testContext.succeeding(merged -> testContext.verify(() -> {

			assertThat(merged).isNotSameAs(target).isNotEqualTo(target);
			target.description = "Source Description";
			assertThat(merged).isEqualTo(target);
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can not merge with a bad description
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldNotMergePublishedNormWithBadDescription(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.description = ValidationsTest.STRING_256;
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", source, profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.description");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can merge the keywords.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldMergePublishedNormKeywords(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.keywords = new ArrayList<>();
		source.keywords.add("Source Keyword");
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.keywords = new ArrayList<>();
		target.keywords.add("Target Keyword");
		target.merge("codePrefix", source, profileManager, testContext.succeeding(merged -> testContext.verify(() -> {

			assertThat(merged).isNotSameAs(target).isNotEqualTo(target);
			target.keywords = new ArrayList<>();
			target.keywords.add("Source Keyword");
			assertThat(merged).isEqualTo(target);
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can not merge with a bad keyword
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldNotMergePublishedNormWithBadKeyword(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.keywords = new ArrayList<>();
		source.keywords.add("    ");
		source.keywords.add(null);
		source.keywords.add(ValidationsTest.STRING_256);
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", source, profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.keywords[2]");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can merge the publisher identifier.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldMergePublishedNormPublisherId(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		profileManager.createProfile(new JsonObject(), testContext.succeeding(profile -> {

			final PublishedNorm source = new PublishedNorm();
			source.publisherId = profile.getString("id");
			final PublishedNorm target = createMinimumValidPublishedNormExample();
			target._id = UUID.randomUUID().toString();
			target.publishTime = TimeManager.now();
			target.publisherId = "Target PublisherId";
			target.merge("codePrefix", source, profileManager, testContext.succeeding(merged -> testContext.verify(() -> {

				assertThat(merged).isNotSameAs(target).isNotEqualTo(target);
				target.publisherId = profile.getString("id");
				assertThat(merged).isEqualTo(target);
				testContext.completeNow();

			})));

		}));

	}

	/**
	 * Check that can not merge with a bad publisher identifier.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldNotMergePublishedNormWithBadPublisherId(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.publisherId = "undefined";
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", source, profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.publisherId");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can merge the norm.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldMergeNormOnPublishedNorm(WeNetProfileManagerService profileManager, VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.norm = new NormTest().createModelExample(3);
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.norm = new NormTest().createModelExample(43);
		target.norm.id = "1";
		target.merge("codePrefix", source, profileManager, testContext.succeeding(merged -> testContext.verify(() -> {

			assertThat(merged).isNotSameAs(target).isNotEqualTo(target);
			target.norm = new NormTest().createModelExample(3);
			target.norm.id = "1";
			assertThat(merged).isEqualTo(target);
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can merge the norm.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldMergeNormOnPublishedNormWithoutNorm(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.norm = new NormTest().createModelExample(3);
		final PublishedNorm target = new PublishedNorm();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", source, profileManager, testContext.succeeding(merged -> testContext.verify(() -> {

			assertThat(merged).isNotSameAs(target).isNotEqualTo(target);
			target.norm = new NormTest().createModelExample(3);
			target.norm.id = merged.norm.id;
			assertThat(merged).isEqualTo(target);
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can not merge with a bad norm
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldNotMergePublishedNormWithBadNorm(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.norm = new Norm();
		source.norm.attribute = ValidationsTest.STRING_256;
		final PublishedNorm target = createMinimumValidPublishedNormExample();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", source, profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.norm.attribute");
			testContext.completeNow();

		})));

	}

	/**
	 * Check that can not merge a bad norm.
	 *
	 * @param profileManager service to manage the user profiles.
	 * @param testContext    context for the test.
	 */
	@Test
	public void shouldNotMergeBadNormOnPublishedNormWithoutNorm(WeNetProfileManagerService profileManager,
			VertxTestContext testContext) {

		final PublishedNorm source = new PublishedNorm();
		source.norm = new Norm();
		source.norm.attribute = ValidationsTest.STRING_256;
		final PublishedNorm target = new PublishedNorm();
		target._id = UUID.randomUUID().toString();
		target.publishTime = TimeManager.now();
		target.merge("codePrefix", source, profileManager, testContext.failing(error -> testContext.verify(() -> {

			assertThat(error).isInstanceOf(ValidationErrorException.class);
			final ValidationErrorException validationError = (ValidationErrorException) error;
			assertThat(validationError.getCode()).isEqualTo("codePrefix.norm.attribute");
			testContext.completeNow();

		})));

	}

}
