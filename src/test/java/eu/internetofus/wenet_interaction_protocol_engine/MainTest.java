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
package eu.internetofus.wenet_interaction_protocol_engine;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.File;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.file.Files;
import java.util.Locale;

import org.apache.commons.cli.Options;
import org.itsallcode.io.Capturable;
import org.itsallcode.junit.sysextensions.SystemErrGuard;
import org.itsallcode.junit.sysextensions.SystemOutGuard;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;
import org.tinylog.Level;

import io.vertx.config.ConfigRetriever;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.junit5.VertxExtension;
import io.vertx.junit5.VertxTestContext;

/**
 * Test the {@link Main}
 *
 * @see Main
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class MainTest {

	/**
	 * Verify the options are localized.
	 *
	 * @param lang language to load the options.
	 */
	@ParameterizedTest(name = "Should create options for locale {0}")
	@ValueSource(strings = { "en", "es", "ca" })
	public void shouldCreateOptionForLocale(String lang) {

		final Locale locale = Locale.getDefault();
		try {

			final Locale newLocale = new Locale(lang);
			Locale.setDefault(newLocale);
			final Main main = new Main();
			final Options options = main.createOptions();
			assertThat(options.hasOption(Main.HELP_OPTION)).isTrue();
			assertThat(options.hasOption(Main.VERSION_OPTION)).isTrue();
			assertThat(options.hasOption(Main.CONF_DIR_OPTION)).isTrue();
			assertThat(options.hasOption(Main.PROPERTY_OPTION)).isTrue();

		} finally {

			Locale.setDefault(locale);
		}
	}

	/**
	 * Verify show help message.
	 *
	 * @param stream captured system output stream.
	 */
	@ExtendWith(SystemOutGuard.class)
	@Test
	public void shouldShowHelpMessage(final Capturable stream) {

		stream.capture();
		Main.main("-" + Main.HELP_OPTION);
		final String data = stream.getCapturedData();
		assertThat(data).contains("-" + Main.HELP_OPTION, "-" + Main.VERSION_OPTION, "-" + Main.CONF_DIR_OPTION,
				"-" + Main.PROPERTY_OPTION);

	}

	/**
	 * Verify show version.
	 *
	 * @param stream captured system err stream.
	 */
	@ExtendWith(SystemErrGuard.class)
	@Test
	public void shouldShowVersion(final Capturable stream) {

		stream.capture();
		Main.main("-" + Main.VERSION_OPTION);
		final String data = stream.getCapturedData();
		assertThat(data).contains(Level.INFO.name());

	}

	/**
	 * Verify undefined argument provokes an error.
	 *
	 * @param stream captured system err stream.
	 */
	@Test
	@ExtendWith(SystemErrGuard.class)
	public void shouldCaptureUndefinedArgument(final Capturable stream) {

		stream.capture();
		Main.main("-undefined");
		final String data = stream.getCapturedData();
		assertThat(data).contains(Level.ERROR.name(), Level.INFO.name());

	}

	/**
	 * Verify error happens when the property parameter is wrong.
	 *
	 * @param stream captured system err stream.
	 */
	@Test
	@ExtendWith(SystemErrGuard.class)
	public void shouldCaptureBadPropertyArgument(final Capturable stream) {

		stream.capture();
		Main.main("-" + Main.PROPERTY_OPTION, "propertyName");
		final String data = stream.getCapturedData();
		assertThat(data).contains(Level.ERROR.name(), Level.INFO.name());

	}

	/**
	 * Verify error happens when the configuration directory parameter is wrong.
	 *
	 * @param stream captured system err stream.
	 */
	@Test
	@ExtendWith(SystemErrGuard.class)
	public void shouldCaptureBadConfDirArgument(final Capturable stream) {

		stream.capture();
		Main.main("-" + Main.CONF_DIR_OPTION);
		final String data = stream.getCapturedData();
		assertThat(data).contains(Level.ERROR.name(), Level.INFO.name());

	}

	/**
	 * Verify can not start server because the port is already binded.
	 *
	 * @param stream captured system err stream.
	 * @param tmpDir temporal directory.
	 *
	 *
	 * @throws Throwable if can not bind a port.
	 */
	@Test
	@ExtendWith(SystemErrGuard.class)
	public void shouldNotStartServerBecausePortIsBidded(final Capturable stream, @TempDir File tmpDir) throws Throwable {

		try (final Socket socket = new Socket()) {

			socket.bind(new InetSocketAddress("localhost", 0));
			final int port = socket.getLocalPort();

			final File confDir = new File(tmpDir, "etc");
			confDir.mkdirs();
			Files.writeString(new File(confDir, "host.json").toPath(),
					"{\"api\":{\"host\":\"localhost\",\"port\":" + port + "}}");

			stream.capture();
			final Thread thread = new Thread(() -> Main.main("-" + Main.CONF_DIR_OPTION, confDir.getAbsolutePath()));
			thread.start();

			String data = stream.getCapturedData();
			for (int i = 0; i < 1000 && !data.contains("Check the Logs to known why."); i++) {

				Thread.sleep(100);
				data = stream.getCapturedData();
			}
			assertThat(data).contains(Level.ERROR.name(), "Check the Logs to known why.");

		}
	}

	/**
	 * Verify can not start server because exist bad configuration files.
	 *
	 * @param stream captured system err stream.
	 * @param tmpDir temporal directory.
	 *
	 *
	 * @throws Throwable if can not create temporal files.
	 */
	@Test
	@ExtendWith(SystemErrGuard.class)
	public void shouldNotStartServerBecauseConfigurationFilesAreWrong(final Capturable stream, @TempDir File tmpDir)
			throws Throwable {

		final File confDir = new File(tmpDir, "etc");
		confDir.mkdirs();
		new File(confDir, "Z").mkdirs();
		final File unreadable = new File(confDir, "x.json");
		unreadable.createNewFile();
		unreadable.setReadable(false);
		Files.writeString(new File(confDir, "bad_yaml.yml").toPath(), "{\"api\":{\"port\":0}}");
		Files.writeString(new File(confDir, "bad_json.json").toPath(), "port:0");

		stream.capture();
		final Thread thread = new Thread(() -> Main.main("-" + Main.CONF_DIR_OPTION, confDir.getAbsolutePath()));
		thread.start();

		String data = stream.getCapturedData();
		for (int i = 0; i < 1000 && !data.contains("Check the Logs to known why."); i++) {

			Thread.sleep(100);
			data = stream.getCapturedData();
		}
		assertThat(data).contains(Level.ERROR.name(), "Check the Logs to known why.");
	}

	/**
	 * Verify capture exception when configure the configuration directory.
	 *
	 * @param testContext context to run the tests.
	 */
	@Test
	@ExtendWith(VertxExtension.class)
	public void shouldCaptureExceptionWhenConfigureDirectory(VertxTestContext testContext) {

		final Main main = new Main();
		testContext.assertFailure(main.startWith("-" + Main.CONF_DIR_OPTION + "undefined://bad/path/to/conf/dir"))
				.setHandler(handler -> testContext.completeNow());
	}

	/**
	 * Verify capture exception when configure the configuration directory.
	 *
	 * @param testContext context to run the tests.
	 */
	@Test
	@ExtendWith(VertxExtension.class)
	public void shouldNotStartBecauseBadAPIPortValue(VertxTestContext testContext) {

		final Main main = new Main();
		testContext.assertFailure(main.startWith("-" + Main.PROPERTY_OPTION + "api.host=\"localhost\"",
				"-" + Main.PROPERTY_OPTION + "api.port=\"80\"")).setHandler(handler -> testContext.completeNow());
	}

	/**
	 * Verify can not start server because cannot start the API.
	 *
	 * @param stream captured system err stream.
	 * @param tmpDir temporal directory.
	 *
	 *
	 * @throws Throwable if can not bind a port for the API.
	 */
	@Test
	@ExtendWith(SystemErrGuard.class)
	public void shouldNotStartServerBecauseAPIVerticleFails(final Capturable stream, @TempDir File tmpDir)
			throws Throwable {

		stream.capture();
		final Thread thread = new Thread(() -> Main.main("-" + Main.PROPERTY_OPTION, "api.port=\"zero\""));
		thread.start();

		String data = stream.getCapturedData();
		for (int i = 0; i < 1000 && !data.contains("Check the Logs to known why."); i++) {

			Thread.sleep(100);
			data = stream.getCapturedData();
		}
		assertThat(data).contains(Level.ERROR.name(), "Check the Logs to known why.");

	}

	/**
	 * Verify can not start server because cannot start the persistence.
	 *
	 * @param stream captured system err stream.
	 * @param tmpDir temporal directory.
	 *
	 * @throws Throwable if can not listen to a bad persistence port.
	 */
	@Test
	@ExtendWith(SystemErrGuard.class)
	public void shouldNotStartServerBecausePersistenceVerticleFails(final Capturable stream, @TempDir File tmpDir)
			throws Throwable {

		stream.capture();
		final Thread thread = new Thread(
				() -> Main.main("-" + Main.PROPERTY_OPTION, "persistence.connection_string=\"undefined connection value\""));
		thread.start();

		String data = stream.getCapturedData();
		for (int i = 0; i < 1000 && !data.contains("Check the Logs to known why."); i++) {

			Thread.sleep(100);
			data = stream.getCapturedData();
		}
		assertThat(data).contains(Level.ERROR.name(), "Check the Logs to known why.");

	}

	/**
	 * Check configuration load from properties.
	 *
	 * @param testContext context to run the tests.
	 *
	 * @throws Throwable if can not create the temporal files.
	 */
	@Test
	@ExtendWith(VertxExtension.class)
	public void shouldLoadConfigurationProperties(VertxTestContext testContext) throws Throwable {

		final Main main = new Main();
		testContext.assertComplete(
				main.startWith("-" + Main.PROPERTY_OPTION + "api.host=\"HOST\"", "-" + Main.PROPERTY_OPTION + "api.port=80",
						"-" + Main.PROPERTY_OPTION, "persistence.db_name=task-manager", "-" + Main.PROPERTY_OPTION,
						"persistence.username=db-user-name", "-" + Main.PROPERTY_OPTION + " persistence.host=phost",
						"-" + Main.PROPERTY_OPTION + "persistence.port=27", "-" + Main.PROPERTY_OPTION,
						"persistence.db_name=DB_NAME", "-" + Main.PROPERTY_OPTION + "persistence.username=USER_NAME",
						"-" + Main.PROPERTY_OPTION + " persistence.password=PASSWORD", "-" + Main.PROPERTY_OPTION,
						"service.webClient.keepAlive=false", "-" + Main.PROPERTY_OPTION, "service.webClient.pipelining=true",
						"-" + Main.VERSION_OPTION))
				.setHandler(handler -> {

					final ConfigRetriever retriever = ConfigRetriever.create(Vertx.vertx(), main.retrieveOptions);
					retriever.getConfig(testContext.succeeding(conf -> testContext.verify(() -> {

						assertThat(conf.getJsonObject("api")).isNotNull();
						assertThat(conf.getJsonObject("api").getString("host")).isEqualTo("HOST");
						assertThat(conf.getJsonObject("api").getInteger("port")).isEqualTo(80);
						assertThat(conf.getJsonObject("persistence")).isNotNull();
						assertThat(conf.getJsonObject("persistence").getString("host")).isEqualTo("phost");
						assertThat(conf.getJsonObject("persistence").getInteger("port")).isEqualTo(27);
						assertThat(conf.getJsonObject("persistence").getString("db_name")).isEqualTo("DB_NAME");
						assertThat(conf.getJsonObject("persistence").getString("username")).isEqualTo("USER_NAME");
						assertThat(conf.getJsonObject("persistence").getString("password")).isEqualTo("PASSWORD");
						assertThat(conf.getJsonObject("service")).isNotNull();
						assertThat(conf.getJsonObject("service").getJsonObject("webClient")).isNotNull();
						assertThat(conf.getJsonObject("service").getJsonObject("webClient").getBoolean("keepAlive", true))
								.isFalse();
						assertThat(conf.getJsonObject("service").getJsonObject("webClient").getBoolean("pipelining", false))
								.isTrue();

						testContext.completeNow();
					})));

				});

	}

	/**
	 * Check configuration load configuration files.
	 *
	 * @param tmpDir      temporal directory.
	 * @param testContext context to run the tests.
	 *
	 * @throws Throwable if can not create the temporal files.
	 */
	@Test
	@ExtendWith(VertxExtension.class)
	public void shouldLoadConfigurationFromFiles(@TempDir File tmpDir, VertxTestContext testContext) throws Throwable {

		final File etc = new File(tmpDir, "etc");
		etc.mkdirs();
		final JsonObject api = new JsonObject().put("host", "HOST").put("port", 80);
		final File apiFile = new File(etc, "api.json");
		apiFile.createNewFile();
		Files.writeString(apiFile.toPath(), new JsonObject().put("api", api).encodePrettily());
		final StringBuilder persistence = new StringBuilder();
		persistence.append("persistence:\n");
		persistence.append("  host: phost\n");
		persistence.append("  port: 27\n");
		persistence.append("  db_name: \"DB_NAME\"\n");
		persistence.append("  username: USER_NAME\n");
		persistence.append("  password: \"PASSWORD\"\n");
		final File persistenceFile = new File(etc, "persistence.yml");
		persistenceFile.createNewFile();
		Files.writeString(persistenceFile.toPath(), persistence.toString());
		final Main main = new Main();
		testContext
				.assertComplete(main.startWith("-" + Main.CONF_DIR_OPTION, etc.getAbsolutePath(), "-" + Main.VERSION_OPTION))
				.setHandler(handler -> {

					final ConfigRetriever retriever = ConfigRetriever.create(Vertx.vertx(), main.retrieveOptions);
					retriever.getConfig(testContext.succeeding(conf -> testContext.verify(() -> {

						assertThat(conf.getJsonObject("api")).isNotNull();
						assertThat(conf.getJsonObject("api").getString("host")).isEqualTo("HOST");
						assertThat(conf.getJsonObject("api").getInteger("port")).isEqualTo(80);
						assertThat(conf.getJsonObject("persistence")).isNotNull();
						assertThat(conf.getJsonObject("persistence").getString("host")).isEqualTo("phost");
						assertThat(conf.getJsonObject("persistence").getInteger("port")).isEqualTo(27);
						assertThat(conf.getJsonObject("persistence").getString("db_name")).isEqualTo("DB_NAME");
						assertThat(conf.getJsonObject("persistence").getString("username")).isEqualTo("USER_NAME");
						assertThat(conf.getJsonObject("persistence").getString("password")).isEqualTo("PASSWORD");

						testContext.completeNow();
					})));

				});

	}

	/**
	 * Check configuration properties are preferred to the defined on the
	 * configuration files.
	 *
	 * @param tmpDir      temporal directory.
	 * @param testContext context to run the tests.
	 *
	 * @throws Throwable if can not create the temporal files.
	 */
	@Test
	@ExtendWith(VertxExtension.class)
	public void shouldConfigureAndUsePropertiesBeforeFiles(@TempDir File tmpDir, VertxTestContext testContext)
			throws Throwable {

		final File etc = new File(tmpDir, "etc");
		etc.mkdirs();
		final JsonObject api = new JsonObject().put("host", "HOST").put("port", 80);
		final File apiFile = new File(etc, "api.json");
		apiFile.createNewFile();
		Files.writeString(apiFile.toPath(), new JsonObject().put("api", api).encodePrettily());
		final StringBuilder persistence = new StringBuilder();
		persistence.append("persistence:\n");
		persistence.append("  host: phost\n");
		persistence.append("  port: 27\n");
		persistence.append("  db_name: \"DB_NAME\"\n");
		persistence.append("  username: USER_NAME\n");
		persistence.append("  password: \"PASSWORD\"\n");
		final File persistenceFile = new File(etc, "persistence.yml");
		persistenceFile.createNewFile();
		Files.writeString(persistenceFile.toPath(), persistence.toString());
		final Main main = new Main();
		testContext.assertComplete(
				main.startWith("-" + Main.CONF_DIR_OPTION + etc.getAbsolutePath(), "-" + Main.PROPERTY_OPTION + "api.port=8081",
						"-" + Main.PROPERTY_OPTION + " persistence.db_name=\"database name\"", "-" + Main.PROPERTY_OPTION,
						"persistence.password=PASSW0RD", "-" + Main.VERSION_OPTION))
				.setHandler(handler -> {

					final ConfigRetriever retriever = ConfigRetriever.create(Vertx.vertx(), main.retrieveOptions);
					retriever.getConfig(testContext.succeeding(conf -> testContext.verify(() -> {

						assertThat(conf.getJsonObject("api")).isNotNull();
						assertThat(conf.getJsonObject("api").getString("host")).isEqualTo("HOST");
						assertThat(conf.getJsonObject("api").getInteger("port")).isEqualTo(8081);
						assertThat(conf.getJsonObject("persistence")).isNotNull();
						assertThat(conf.getJsonObject("persistence").getString("host")).isEqualTo("phost");
						assertThat(conf.getJsonObject("persistence").getInteger("port")).isEqualTo(27);
						assertThat(conf.getJsonObject("persistence").getString("db_name")).isEqualTo("database name");
						assertThat(conf.getJsonObject("persistence").getString("username")).isEqualTo("USER_NAME");
						assertThat(conf.getJsonObject("persistence").getString("password")).isEqualTo("PASSW0RD");

						testContext.completeNow();
					})));

				});

	}

	/**
	 * Verify can not start server because cannot start the service.
	 *
	 * @param stream captured system err stream.
	 * @param tmpDir temporal directory.
	 *
	 * @throws Throwable if can not listen to a bad persistence port.
	 */
	@Test
	@ExtendWith(SystemErrGuard.class)
	public void shouldNotStartServerBecauseServiceVerticleFails(final Capturable stream, @TempDir File tmpDir)
			throws Throwable {

		stream.capture();
		final Thread thread = new Thread(() -> Main.main("-" + Main.PROPERTY_OPTION, "service.webClient.keepAlive=false",
				"-" + Main.PROPERTY_OPTION, "service.webClient.pipelining=true"));
		thread.start();

		String data = stream.getCapturedData();
		for (int i = 0; i < 1000 && !data.contains("Check the Logs to known why."); i++) {

			Thread.sleep(100);
			data = stream.getCapturedData();
		}
		assertThat(data).contains(Level.ERROR.name(), "Check the Logs to known why.");

	}

}
